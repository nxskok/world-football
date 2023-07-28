
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
  # so far like https://www.worldfootball.net/all_matches/swe-allsvenskan-2023/
  # print(fname)
  fname <- str_replace(fname, "all_matches", "schedule")
  # print(fname)
  fname <- str_replace(fname, "/$", "-spieltag/99/")
  # print(fname)
  fname
  # Sys.sleep(1)
  # html <- read_html(my_url)
  # html %>% html_children() %>%
  #   pluck(2) %>% # body
  #   html_elements('div.navibox2') %>%
  #   html_children() %>%
  #   html_children() %>%
  #   pluck(2) %>%
  #   html_nodes('a') %>%
  #   pluck(1) %>%
  #   html_attr('href') -> sched_url
  # sched_url
}

get_league_table <- function(my_url) {
#  my_url <- str_c("https://www.worldfootball.net", my_url)
  Sys.sleep(1)
  table_html <- read_html(my_url)
  table_html %>% html_nodes('table.standard_tabelle') %>%
    pluck(1) %>% # there might be a third one if there are two divisions / conferences
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
    mutate(team = str_replace(team, "^\\n", "")) %>%
    mutate(team = str_replace(team, "\\n.*$", "")) %>%
    mutate(team = str_replace(team, "\\n$", "")) -> table
  table
  # return(table)
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
  lt$ranks
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
  games %>%
    left_join(lu_table, by = c("t1" = "team")) %>%
    left_join(lu_table, by = c("t2" = "team")) %>%
    rowwise() %>%
    mutate(ppd = list(make_ppd(rdsname, id.x, id.y)))
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
    arrange(desc(pt), desc(gd)) %>% mutate(rank = row_number()) -> sim_result
  # return(sim_result)
  # sim_result
  map(lt$ranks, \(x) teams_within_rank(sim_result, x)) -> top_teams
  names(top_teams) = as.character(lt$ranks)
  top_teams
}

