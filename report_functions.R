

what_to_do <- function(rdsname, cutoff_date) {
  fname <- str_remove(rdsname, "\\.rds$")
  v <- list.files(path = "sim", pattern = fname, full.names = TRUE)
  if (length(v) == 0) return(character(0))
  enframe(v, name = NULL, value = "filename") %>%
    mutate(mtime = file.mtime(filename)) %>%
    mutate(ba = ifelse(mtime <= cutoff_date, "before", "after")) -> vv
  # if no afters, nothing to do
  if (!any(str_detect(vv$ba, "after"))) return(character(0))
  # if no befores, likewise
  if (!any(str_detect(vv$ba, "before"))) return(character(0))
  # there are some of each. I want the latest before and the earliest after
  vv %>% group_by(ba) %>%
    summarize(min_m = min(filename), max_m = max(filename)) -> vv_summ
  # after is first, before second
  latest_before <- vv_summ$max_m[2]
  earliest_after <- vv_summ$min_m[1]
  ans <- c(latest_before, earliest_after)
  ans
}

make_changes <- function(filenames) {
  read_rds(filenames[1]) %>% janitor::clean_names() -> p1
  read_rds(filenames[2]) %>% janitor::clean_names() -> p2
  # add a column r if there isn't one
  has_r1 <- ("r" %in% names(p1))
  has_r2 <- ("r" %in% names(p2))
  if (!has_r1) {
    p1 %>% mutate(r = row_number()) -> p1
  }
  if (!has_r2) {
    p2 %>% mutate(r = row_number()) -> p2
  }
  p1 %>% left_join(p2, by = "team") %>%
    select(team, starts_with("x")) -> dd
  if (ncol(dd) < 2) return(NULL)
  # if (ncol(p1) != ncol(p2)) return(NULL)
  dd %>%
    pivot_longer(-team, names_to = c("rank", ".value"), names_sep = "\\.") %>%
    mutate(change = y - x) %>%
    select(team, rank, change) %>%
    pivot_wider(names_from = rank, values_from = change) -> changes
  changes
}

tables_for_report <- function(rdsname, cutoff_date) {
  filenames <- what_to_do(rdsname, cutoff_date)
  # print(glue::glue("rdsname is {rdsname}, filename: {filenames}"))
  if (length(filenames) == 0) return(list())
  changes <- make_changes(filenames)
  games <- read_rds(str_c("rds/", rdsname))
  filenames %>% file.mtime() -> mtimes
  games %>% filter(between(ko, mtimes[1]-hours(3), now())) -> games
  tab_recent <- read_rds(filenames[2])
  tab_recent %>% mutate(r = row_number()) -> tab_recent
  tab_recent %>% select(r, everything()) %>% select(-played) %>% rename(mn_rk = mean_rank) -> tab_recent
  return(list(changes = changes, games = games, recent = tab_recent, mtimes = mtimes))
}
