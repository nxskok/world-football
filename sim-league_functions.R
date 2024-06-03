
make_url <- function(rdsname) {
  # a single rdsname, not a vector
  str_replace(rdsname, ".rds$", "") %>%
    str_replace_all("_", "-") %>%
    str_c("https://www.worldfootball.net/all_matches/", ., "/") -> fname
  return(fname)
}

#
get_league_table_url <- function(rdsname) {
  # url like this aimed for: https://www.worldfootball.net/schedule/swe-allsvenskan-2023-spieltag/99/
  fname <- make_url(rdsname)
  fname <- str_replace(fname, "all_matches", "schedule")
  fname <- str_replace(fname, "/$", "-spieltag/99/")
  if (str_detect(rdsname, "^ger_"))
    fname <- str_replace(fname, "/ger-", "/")
  if (str_detect(rdsname, "rus_1-division"))
    fname <- str_replace(fname, "2023-2024", "2023-24")
  # print(fname)
  fname
}

get_league_table <- function(my_url) {
  # print(my_url)
#  my_url <- str_c("https://www.worldfootball.net", my_url)
  Sys.sleep(1)
  table_html <- read_html(my_url)
  table_html %>% html_nodes('table.standard_tabelle') %>%
    pluck(1) %>% # there might be an extra one if there are two divisions / conferences
    html_nodes('td') -> tds
  tds
  tds %>% html_text() -> texts
  texts
  tds %>% html_attr("bgcolor") -> colours
  nt <- length(texts)  / 10
  # print(nt)
  tibble(text = texts) %>%
    mutate(row = gl(nt, 10)) %>%
    mutate(col = gl(10, 1, nt*10)) %>%
    filter(col %in% c(3, 4, 9, 10)) %>%
    pivot_wider(names_from = col, values_from = text) %>%
    rename(team = "3", played = "4", gd = "9", pts = "10") %>%
    mutate(team = str_remove(team, "^\\n")) %>%
    mutate(team = str_remove(team, "\\n.*$")) %>%
    mutate(team = str_remove(team, "\\n$")) -> table
  # table %>% # mutate(r = row_number()) %>%
  #   select(r, everything()) -> table
  # # return(table)
  enframe(colours, name = NULL) %>%
    mutate(row = gl(nt, 10)) %>%
    mutate(col = gl(10, 1, nt*10)) %>%
    filter(col == 1) %>%
    select(-col) %>%
    mutate(next_col = lead(value, 1)) %>%
    mutate(is_same = (next_col == value)) %>%
    filter(!is_same) %>% pull(row) %>% as.numeric() -> ranks
  list(table = table, ranks = ranks)
}

league_table_from_rdsname <- function(rdsname) {
  lt_url <- get_league_table_url(rdsname)
  lt <- get_league_table(lt_url)
  if (1 %in% lt$ranks) {} else {lt$ranks = c(1, lt$ranks)}
  lt$table %>%
    select(team, g = played, pt = pts, gd) %>%
    mutate(across(g:gd, \(x) as.numeric(x))) -> ltt
  list(table = ltt, ranks = lt$ranks)
}

get_unplayed <- function(rdsname) {
  read_rds(str_c("rds/", rdsname)) %>%
    filter(is.na(score)) %>%
    select(-score)
}

get_with_ppd <- function(rdsname, games) {
  read_rds(str_c("lu/", rdsname)) -> lu_table
  # lu_table
  suppressWarnings({
    games %>%
      left_join(lu_table, by = c("t1" = "team")) %>%
      left_join(lu_table, by = c("t2" = "team")) %>%
      rowwise() %>%
      mutate(ppd = list(make_ppd(rdsname, id.x, id.y))) -> a
  })
  a
}

gd_from_score <- function(score) {
  v <- as.numeric(str_split_1(score, "-"))
  c(v[1]-v[2], v[2]-v[1])
}

points_from_score <- function(score) {
  # single score (not vectorized)
  v <- str_split_1(score, "-")
  case_when(
    v[1] > v[2] ~ c(3, 0),
    v[1] < v[2] ~ c(0, 3),
    .default    = c(1, 1)
  )
}

teams_within_rank <- function(sim, r) {
  sim %>% filter(rank <= r) %>%
    pull(team)
}



sample_from_ppd <- function(with_ppd, lt) {
  with_ppd %>% mutate(score = with(ppd, sample(score, 1, prob = p))) %>%
    mutate(points = list(points_from_score(score))) %>%
    unnest_wider(points, names_sep = "_") %>%
    rowwise() %>%
    mutate(gd = list(gd_from_score(score))) %>%
    unnest_wider(gd, names_sep = "_") %>%
    select(t1, t2, points_1, points_2, gd_1, gd_2) -> sim
  sim %>% select(ends_with("1")) -> sim1
  sim %>% select(ends_with("2")) -> sim2
  sim1 %>% group_by(team = t1) %>%
    summarize(g = n(), pt = sum(points_1), gd = sum(gd_1)) -> sum1
  sim2 %>% group_by(team = t2) %>%
    summarize(g = n(), pt = sum(points_2), gd = sum(gd_2)) -> sum2
  bind_rows(lt$table, sum1, sum2) %>%
    group_by(team) %>%
    summarize(across(g:gd, \(x) sum(x))) %>%
    arrange(desc(pt), desc(gd)) %>% mutate(rank = row_number())
}

sample_many_from_ppd <- function(with_ppd, lt, n_sim = 1000) {
  tibble(sim_count = 1:n_sim) %>%
    rowwise() %>%
    mutate(sim = list(sample_from_ppd(with_ppd, lt))) %>%
    pull(sim) %>% bind_rows() -> dd

  dd %>%
    group_by(team) %>%
    summarize(mean_rank = mean(rank)) %>%
    arrange(mean_rank) %>%
    mutate(r = row_number())-> mean_ranks

  dd %>%
    rowwise() %>%
    mutate(cutoff = list(lt$ranks)) %>%
    unnest(cutoff) %>%
    mutate(is_in = rank <= cutoff) %>%
    group_by(team, cutoff) %>%
    summarise(total = sum(is_in)) %>%
    pivot_wider(names_from = cutoff, values_from = total) -> d3 # or keep it long for now

  mean_ranks %>% left_join(lt$table, join_by(team)) %>% left_join(d3, join_by(team))
  # mean_ranks %>% left_join(lt$table) %>% left_join(d3)
}

make_no_sim <- function(lt) {
  lt$table %>%
    mutate(rank = row_number()) %>%
    group_by(team) %>%
    summarize(mean_rank = mean(rank)) %>%
    arrange(mean_rank) -> mean_ranks
  lt$table %>%
    mutate(rank = row_number()) %>%
    rowwise() %>%
    mutate(cutoff = list(lt$ranks)) %>%
    unnest(cutoff) %>%
    mutate(is_in = (rank <= cutoff)) %>%
    group_by(team, cutoff) %>%
    summarize(total = 1000 * sum(is_in)) %>%
    pivot_wider(names_from = cutoff, values_from = total) -> d3
  mean_ranks %>% left_join(lt$table, join_by(team)) %>% left_join(d3, join_by(team))
}

make_sim_fname <- function(rdsname) {
  now <- now()
  fname <- str_replace(rdsname, "\\.rds$", str_c("_", now, ".rds"))
  str_c("sim/", fname)
}

sample_from_rdsname <- function(rdsname, n_sim = 1000) {
  # print(rdsname)
  # print(now())
  # next line from logr
  # log_print(rdsname, console = FALSE)
  ltt <- league_table_from_rdsname(rdsname) # has table in table, ranks to sim for in ranks
  games <- get_unplayed(rdsname)
  if (nrow(games) > 0) {
    with_ppd <- get_with_ppd(rdsname, games)
    sample_many_from_ppd(with_ppd, ltt, n_sim) -> sim
  } else {
    sim <- make_no_sim(ltt)
  }
  fname <- make_sim_fname(rdsname)
  write_rds(sim, fname)
  log_print(glue::glue("Done {rdsname}"), console = FALSE)
  sim
}

last_sim <- function(rdsname) {
  fname <- str_replace(rdsname, "\\.rds$", "")
  v <- list.files(path = "sim", pattern = fname, full.names = TRUE)
  if (length(v) == 0) return(NA)
  v %>% file.mtime() %>% max()
}

sim_as_needed <- function(leagues) {
  # print(now())
  leagues %>% rowwise() %>%
    mutate(fname = make_fname(country, league, season, part, prefix = "")) %>%
    select(fname) %>%
    mutate(last_simul = last_sim(fname)) %>%
    mutate(last_fit = file.mtime(str_c("fit/", fname))) %>%
    mutate(fit_size = file.size(str_c("fit/", fname))) %>%
    mutate(needs_doing = case_when(
      is.na(last_fit)   ~ "no",
      fit_size < 100    ~ "no",
      is.na(last_simul) ~ "yes",
      last_fit > last_simul ~ "yes",
      .default = "no"
    )) %>%
    filter(needs_doing == "yes") %>%
    ungroup() %>%
    slice_sample(prop = 1) %>%
    pull(fname) -> fnames
  # print(fnames)
  # print("")
  log_print(fnames)
  fnames %>% walk(\(x) sample_from_rdsname(x), .progress = TRUE)
  now()
}
