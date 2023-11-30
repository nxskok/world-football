get_prediction_times <- function() {
  list.files(pattern = "preds2*") %>%
    enframe() %>%
    mutate(tt1 = str_remove(value, "^preds")) %>%
    mutate(tt = str_remove(tt1, ".rds$")) %>%
    mutate(pred_time = as_datetime(tt, tz = "America/Toronto")) %>%
    select(filename = value, pred_time)
}


get_all_predictions <- function(predfiles) {
  predfiles$filename %>%
    map(\(x) read_rds(x)) %>%
    bind_rows(.id = "id") %>%
    mutate(game_key = str_c(t1, "_", t2, "_", ko)) %>%
    group_by(game_key) %>%
    filter(id == max(id)) %>%
    select(game_key, `2`, `1`, `0`)
}


get_all_results <- function(leagues) {
  map(1:nrow(leagues), \(x) display_league(leagues, x)) %>%
    bind_rows() %>%
    drop_na(score) %>%
    unite(game_key, t1, t2, ko, sep = "_")
}

combine_resprob <- function(all_predictions, all_results) {
  all_predictions %>%
    left_join(all_results) %>%
    janitor::clean_names() %>%
    filter(str_detect(score, "-")) %>%
    separate_wider_delim(score, " - ", names = c("s1", "s2")) %>%
    mutate(s1 = as.numeric(s1), s2 = as.numeric(s2)) %>%
    ungroup()
}


calc_loglik <- function(res_prob) {
  res_prob %>%
    ungroup() %>%
    mutate(prob = case_when(
      s1 > s2  ~ x2,
      s1 == s2 ~ x1,
      s1 < s2  ~ x0
    )) %>%
    summarize(loglik = sum(log(prob/100))) %>%
    pull(loglik)
}

sim1_loglik <- function(res_prob) {
  res_prob %>%
    mutate(rr = runif(nrow(res_prob))*100) %>%
    mutate(simprob = case_when(
      rr < x2      ~ x2,
      rr < x2 + x1 ~ x1,
      .default = x0
    )) %>%
    summarize(loglik = sum(log(simprob / 100))) %>%
    pull(loglik)
}

sim_loglik <- function(nsim, res_prob) {
  tibble(sim = 1:nsim) %>%
    rowwise() %>%
    mutate(loglik = sim1_loglik(res_prob))
}
