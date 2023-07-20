

rate <- function(fname) { # rds filename with results in it, no folder
  pp <- cmdstan_model("rate_poisson.stan")
  my_fname <- str_c("rds/", fname)
  games <- read_rds(my_fname)
  # lookup table
  with(games, enframe(c(t1, t2), name = NULL, value = "team")) %>%
    count(team) %>%
    select(-n) %>%
    mutate(id = row_number()) -> lookup_table
  # keep only actual games
  games %>% drop_na(score) %>%
    separate(score, into = c("s1", "s2"), convert = TRUE) -> games
  if (nrow(games)==0) {
    ll <- list(fit = NA, teams = lookup_table)
    write_rds(ll, str_c("fit/", fname))
    return(ll)
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
  ll <- list(fit = fit, teams = lookup_table)
  write_rds(ll, str_c("fit/", fname))
  ll
}

rating_table <- function(fit_object) {
  fit <- fit_object$fit
  if (typeof(fit) == "logical") stop("no games played yet")
  lookup_table <- fit_object$teams
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
