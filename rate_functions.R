

rate <- function(fname) { # rds filename with results in it, no folder
  if (file.exists("pp.rds")) {
    pp <- read_rds("pp.rds")
  } else {
    pp <- cmdstan_model("rate_poisson.stan")
    write_rds(pp, "pp.rds")
  }
  my_fname <- str_c("rds/", fname)
  games <- read_rds(my_fname)
  # lookup table
  with(games, enframe(c(t1, t2), name = NULL, value = "team")) %>%
    count(team) %>%
    select(-n) %>%
    mutate(id = row_number()) -> lookup_table
  # save lookup table
  my_lname <- str_c("lu/", fname)
  write_rds(lookup_table, my_lname)
  # keep only actual games
  games %>% drop_na(score) %>%
    separate(score, into = c("s1", "s2"), convert = TRUE) -> games
  if (nrow(games)==0) {
    fit = NA
    write_rds(fit, str_c("fit/", fname))
    return(list(fit = fit, teams = lookup_table))
  }
  # otherwise continue
  games %>% left_join(lookup_table, by = c("t1" = "team")) %>%
    left_join(lookup_table, by = c("t2" = "team")) -> ingredients
  stan_data <- with(ingredients, list(
    nt = nrow(lookup_table),
    ng = nrow(games),
    x = cbind(id.x, id.y),
    y = cbind(s1, s2)))
  fit <- pp$sample(data = stan_data)
  fit$save_object(str_c("fit/", fname))
  ll <- list(fit = fit, teams = lookup_table)
  ll
}

rating_table <- function(fname) {
  fit <- read_rds(str_c("fit/", fname))
  if (typeof(fit) == "logical") stop("no games played yet")
  lookup_table <- read_rds(str_c("lu/", fname))
  fit$draws(format = "df") %>%
    select(-lp__, -.chain, -.iteration, -.draw) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarize(post_mean = mean(value)) %>%
    extract(name, into = c("what", "id"), "(.)\\[(\\d+)\\]", convert = TRUE) %>%
    drop_na(what) %>%
    pivot_wider(names_from = "what", values_from = "post_mean") %>%
    left_join(lookup_table) %>%
    mutate(rate = o + d) %>%
    select(team, rate, o, d) %>%
    arrange(desc(rate))
}

update_rate <- function(leagues) {
  leagues %>% mutate(fname1 = make_fname(country, league, season, part)) %>%
    mutate(fname2 = str_replace(fname1, "rds/", "fit/")) %>%
    mutate(across(starts_with("fname"), \(f) file.mtime(f), .names = "mtime_{.col}")) %>%
    mutate(needs_update = case_when(
      is.na(mtime_fname2) ~ "yes",
      is.na(mtime_fname1) ~ "no",
      mtime_fname1 > mtime_fname2 ~ "yes",
      .default = "no"
    )) %>%
    filter(needs_update == "yes") -> nn
  print(glue::glue("Needing update: {nrow(nn)}"))
  nn %>%
    # slice_sample(n = 20) %>%
    pull(fname1) -> todos
  if (length(todos) == 0) stop("Nothing to do")
  print(todos)
  todos %>%
    str_replace("rds/", "") %>%
    walk(\(x) rate(x))
}

