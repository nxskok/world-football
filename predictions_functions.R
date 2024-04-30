# predictions functions

make_ppd <- function(fname, t1, t2) {
  teams_to_pred <- as.character(c(t1, t2))
  cols_wanted <-
    c(str_c("o[", teams_to_pred, "]"),
      str_c("d[", teams_to_pred, "]"),
      "h")
  fit <- read_rds(str_c("fit/", fname))
  if (typeof(fit) == "logical") stop("no ratings yet")
  dw <- fit$draws(format = "df")
  nr <- nrow(dw)
  dw %>% select(all_of(cols_wanted)) %>%
    mutate(hm = exp(get(cols_wanted[1]) - get(cols_wanted[4]) + h),
           aw = exp(get(cols_wanted[2]) - get(cols_wanted[3]))) %>%
    mutate(hh = rpois(nr, hm), aa = rpois(nr, aw)) %>%
    count(hh, aa) %>%
    mutate(p = n / nr) %>%
    arrange(desc(p)) %>%
    unite("score", hh, aa,  sep = "-") %>%
    select(-n)
}


best_match <- function(x, lookup) {
  stringsimmatrix(x, lookup$team, method = "lcs") %>%
    apply(1, which.max) -> perm
  lookup$team[perm]
}

#
result_of <- function(tscore) {
  # get result of text score, vectorized
  enframe(tscore, name = NULL, value = "score") %>%
    separate(score, into = c("s1", "s2"), convert = TRUE) %>%
    mutate(result = case_when(
      s1 > s2 ~ "2",
      s1 < s2 ~ "0",
      .default = "1"
    )) %>%
    pull(result)
}
# result_of(c("2-2", "2-1", "0-1"))

ept <- function(pred, ppd, results, scores) {
  ppd %>%
    mutate(predn = pred) %>%
    mutate(r_score = result_of(score)) %>%
    mutate(r_predn = result_of(predn)) %>%
    mutate(pt = 1 * (r_score == r_predn) +
             2 * (score == predn) +
             2 * (r_score == r_predn) * (str_detect(results, r_score)) +
             2 * (score == predn) * (!str_detect(scores, score))
    ) %>%
    summarize(ept = sum(pt * p)) %>%
    pull(ept)
}
# my_score <- "2-1"
# my_res <- "0"
# my_scores <- "1-0:2-1:2-0:1-1:3-1:2-2:1-2"
# ept(my_score, ppd, my_res, my_scores)


max_ept <- function(ppd, results, scores) {
  crossing(x = 0:6, y = 0:6) %>%
    unite(psc, x, y, sep = "-") %>%
    rowwise() %>%
    mutate(my_ept = ept(psc, ppd, results, scores)) %>%
    ungroup() %>%
    slice_max(my_ept, n = 1, with_ties = FALSE) %>%
    pull(psc)
}
# max_ept(ppd, my_res, my_scores)

make_result_probs <- function(fname, t1, t2) {
  if (is.na(t1) || is.na(t2)) {
    nas <-rep(NA, 3L)
    names(nas) <- 0:2
    return(nas)
  }
  make_ppd(fname, t1, t2) %>%
    separate(score, into = c("s1", "s2"), convert = TRUE) %>%
    mutate(result = case_when(
      s1 > s2 ~ 2,
      s1 < s2 ~ 0,
      .default = 1
    )) %>%
    group_by(result) %>%
    summarize(pp = sum(p)) %>%
    mutate(ppp = 100 * round(pp, 2)) %>%
    select(-pp) %>%
    deframe()
}

pop_scores_old <- function(fname, t1, t2) {
  name_vec <- paste0("s", 1:2)
  if (is.na(t1) || is.na(t2)) {
    nas <-rep(NA, 2L)
    names(nas) <- name_vec
    return(nas)
  }
  d <- make_ppd(fname, t1, t2)
  v <- d$score[1:2]
  names(v) <- name_vec
  v
}

ppd_scores <- function(fname, t1, t2) {
  if (is.na(t1) || is.na(t2)) {
    return(tibble(score = "0-0", p = 1, rank = 1))
  }
  make_ppd(fname, t1, t2) %>% mutate(rank = row_number())
}


league_preds <- function(fname) {
  print(fname)
  lu_name <- str_c("lu/", fname)
  rds_name <- str_c("rds/", fname)
  lu <- read_rds(lu_name)
  read_rds(rds_name) %>%
    # filter(is.na(score)) %>%
    filter(between(ko, now() - hours(3),  now() + days(3))) -> dd
  if (nrow(dd) == 0) return(NULL)
  dd %>%
    left_join(lu, by = c("t1" = "team")) %>%
    left_join(lu, by = c("t2" = "team")) %>%
    rowwise() %>%
    mutate(pr = list(make_result_probs(fname, id.x, id.y))) %>%
    mutate(ppd = list(ppd_scores(fname, id.x, id.y))) %>%
    unnest_wider(pr) %>%
    mutate(fname = fname) %>%
    select(fname, ko, t1, t2, `2`, `1`, `0`, ppd)
}
#
#

update_predictions <- function(leagues) {
  my_fname <- "predictions.rds"
  my_xname <- str_c("preds", now(), ".rds")
  file.copy(my_fname, my_xname)
  leagues %>% mutate(r = row_number()) %>%
    rowwise() %>%
    mutate(fname = make_fname(country, league, season, part, prefix = "")) %>%
    mutate(mtime = file.mtime(str_c("fit/", fname))) %>%
    mutate(size = file.size(str_c("fit/", fname))) %>%
    filter(!is.na(mtime)) %>%
    filter(size > 100) %>%
    ungroup() %>%
    slice_sample(n = 999) %>%
    pull(fname) %>%
    map(\(x) league_preds(x)) %>% bind_rows() %>% arrange(ko) -> predictions
  # read all_ranks
  all_ranks <- read_rds("all_ranks.rds")
  predictions %>%
    left_join(all_ranks, join_by(fname, t1 == team)) %>%
    left_join(all_ranks, join_by(fname, t2 == team)) %>%
    select(fname, ko, t1, r1 = rk.x, r2 = rk.y, t2, `2`, `1`, `0`, ppd) -> predictions2
  write_rds(predictions2, "predictions.rds")
}
