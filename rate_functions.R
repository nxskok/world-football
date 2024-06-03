

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
  # fit <- pp$sample(data = stan_data, save_cmdstan_config = TRUE, save_metric = TRUE)
  fit <- pp$sample(data = stan_data)
  fit$save_object(str_c("fit/", fname))
  ll <- list(fit = fit, teams = lookup_table)
  ll
}

long_draws <- function(fname) {
  fit <- read_rds(str_c("fit/", fname))
  if (typeof(fit) == "logical") stop("no games played yet")
  fit$draws(format = "df") %>%
    select(-lp__, -.chain, -.iteration) %>%
    pivot_longer(-.draw)
}


rating_table <- function(fname) {
  lookup_table <- read_rds(str_c("lu/", fname))
  long_draws(fname) %>%
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
  leagues %>% rowwise() %>%
    mutate(fname1 = make_fname(country, league, season, part, prefix = "rds")) %>%
    mutate(fname2 = make_fname(country, league, season, part, prefix = "fit")) %>%
    mutate(fit_size = file.size(fname2)) %>%
    mutate(across(starts_with("fname"), \(f) file.mtime(f), .names = "mtime_{.col}")) %>%
    mutate(needs_update = case_when(
      is.na(mtime_fname1) ~ "no",
      is.na(mtime_fname2) ~ "yes",
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



next_rate_date <- function(rdsname) {
  fit_name <- str_c("fit/", rdsname)
  game_name <- str_c("rds/", rdsname)
  no_rate_date <- ymd_hms("2100-01-01 05:00:00")
  if (!file.exists(fit_name)) {
    if (!(file.exists(game_name))) return(no_rate_date)
    rat_mtime <- ymd_hms("1900-01-01 05:00:00")
  } else {
    rat_mtime <- file.mtime(fit_name)
  }
  games <- read_rds(str_c("rds/", rdsname))
  games %>% filter(ko > rat_mtime) -> d
  # d contains all games with kickoff since last rating time
  if (nrow(d) == 0) return(no_rate_date)
  d %>%
    arrange(ko) %>%
    pluck("ko", 1) -> first_ko
  with_tz(first_ko + hours(2), "America/Toronto")
#
#   %>%
#     pivot_longer(t1:t2) %>%
#     mutate(dup = duplicated(value)) -> dd
#   # dd is d made long with an extra column indicating if duplicate
#   dd %>% filter(dup) -> dd2
#   if (nrow(dd2) == 0) return(max(d$ko) + hours(2))
#   dd2 %>% summarize(min_ko = min(ko)) %>%
#     pull(min_ko) -> first_dup
#   # first_dup is kickoff time of first game with duplicate team
#   if (is.na(first_dup)) return(max(d$ko) + hours(2))
#   dd %>% filter(ko < first_dup) -> ddd
#   ### ddd is all games with kickoff before the first duplicated one
#   if (nrow(ddd) == 0) return(first_dup + hours(2))
#   # I want: 5pm on day before first duplicate (might be late, but should reliably work)
#   ddd %>%
#     summarize(max_ko = max(ko) + hours(2)) %>%
#     pull(max_ko) -> max_ko
#   if (hour(first_dup) > 17) {
#     hour(first_dup) <- 17
#     minute(first_dup) <- 2
#   } else {
#     hour(first_dup) <- 17
#     minute(first_dup) <- 2
#     first_dup <- first_dup - days(1)
#   }
#   # earlier of first_dup and max_ko
#   min(first_dup, max_ko)
}

all_next_rate_date <- function(leagues) {
  leagues %>%
    rowwise() %>%
    mutate(fname = make_fname(country, league, season, part, prefix = "")) %>%
    # select(fname) %>%
    mutate(next_rate = next_rate_date(fname),
           next_rate = with_tz(next_rate, "America/Toronto"),
           dow = weekdays(next_rate)) %>%
    arrange(next_rate)
}

make_rank_table <- function(fname) {
  fitname <- str_c("fit/", fname)
  rat <- read_rds(fitname)
  if (length(rat) == 1 && is.na(rat)) return(NULL)
  rat %>% as_draws_df() %>%
    summarise_draws("mean") %>%
    filter(str_detect(variable, "^[od]")) %>%
    separate_wider_position(variable, widths = c(od = 1, num_txt = 4), too_few = "align_start") %>%
    mutate(id = parse_number(num_txt)) %>%
    group_by(id) %>%
    summarize(rat = sum(mean)) %>%
    mutate(rk = rank(-rat)) %>%
    select(id, rk) -> rank_table
  lu <- read_rds(str_c("lu/", fname))
  lu %>% left_join(rank_table, join_by("id")) %>%
    select(team, rk) -> team_ranks
  team_ranks
}

make_all_ranks <- function(leagues) {
  leagues %>%
    # slice_sample(n = 5) %>%
    rowwise() %>%
    mutate(fname = make_fname(country, league, season, part, prefix = ""))%>%
    select(fname) %>%
    mutate(fit_exists = file.exists(str_c("fit/", fname))) %>%
    filter(fit_exists) %>%
    mutate(rank_table = list(make_rank_table(fname))) %>%
    unnest(rank_table) -> all_ranks
  # save all_ranks (used below)
  write_rds(all_ranks, "all_ranks.rds")
  all_ranks
}
