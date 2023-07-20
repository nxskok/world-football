# functions for worldfootball.net

make_fname <- function(x, y, z, w) {
  fn <- str_c("rds/", x, "_", y, "_", z)
  fn <- ifelse(is.na(w),
               str_c(fn, ".rds"),
               str_c(fn, "_", w, ".rds"))
  fn
}


get_schedule <- function(country_name, league_name, season, part) {
  Sys.sleep(1)
  my_url <- str_c("https://www.worldfootball.net/all_matches/", country_name, "-", league_name, "-", season)
  my_url <- ifelse(is.na(part),
                   str_c(my_url, "/"),
                   str_c(my_url, "-", part, "/"))
  my_url <- str_replace(my_url, "/ger-", "/")
  fname <- make_fname(country_name, league_name, season, part)
  print(glue::glue("Getting {fname}."))
  h <- read_html(my_url)
  h %>% html_node(xpath = '//*[@id="site"]/div[2]/div[1]/div[1]/div[3]/div/table') %>%
    html_table(header = FALSE) %>%
    filter(!str_detect(X1, "Round")) %>%
    mutate(X1 = ifelse(X1 == "", NA, X1)) %>%
    fill(X1) %>%
    mutate(kostr = str_c(X1, " ", X2)) %>%
    mutate(ko1 = dmy_hm(kostr, tz = "Europe/London")) %>%  # it looks as if it's UK time
    mutate(ko1 = with_tz(ko1, "America/Toronto")) %>%
    mutate(ko2 = dmy(X1, tz = "Europe/London")) %>%
    mutate(ko2 = with_tz(ko2, "America/Toronto")) %>%
    mutate(ko = coalesce(ko1, ko2)) %>%
    select(ko, t1 = X3, t2 = X5, score0 = X6) %>%
    extract(score0, into = c("s1", "s2"), "^(\\d+):(\\d+)") %>%
    mutate(score = str_c(s1, " - ", s2)) %>%
    select(-s1, -s2) %>%
    write_rds(fname) -> x
  fname
}


leagues <- read_csv("leagues.csv")


get_schedule_row <- function(n, leagues) {
  with(leagues, get_schedule(country[n], league[n], season[n], part[n]))
}

download_these <- function(l) {
  nr <- nrow(l)
  if (nr == 0) stop("no leagues to download")
  walk(1:nr, \(i) get_schedule_row(i, l))
}


display_league <- function(leagues, n) {
  if (nrow(leagues) == 0) stop ("no rows to display")
  leagues %>% slice(n) %>%
    mutate(fname = make_fname(country, league, season, part)) %>%
    pull(fname) -> filename
  read_rds(filename)
}

find_games <- function(leagues, start_time, end_time) {
  if (nrow(leagues) == 0) stop ("no rows to display")
  leagues %>% mutate(r = row_number()) %>%
    rowwise() %>%
    mutate(games = list(display_league(leagues, r))) %>%
    unnest(games) %>%
    drop_na(ko) %>%
    filter(between(ko, start_time, end_time))
}

update_leagues <- function(leagues) {
  leagues %>%
    mutate(r = row_number()) %>%
    mutate(fname = make_fname(country, league, season, part)) %>%
    mutate(mtime = file.mtime(fname)) %>%
    rowwise() %>%
    mutate(gg = list(display_league(leagues, r))) %>%
    unnest(gg) %>%
    filter(ko - hours(3) > mtime) %>%
    filter(ko < now()) -> gg1
  gg1 %>%
    nest_by(country, league, season, part) %>%
    select(-data) -> to_get
  if (nrow(to_get) == 0) stop("No leagues to get.")
  download_these(to_get)
  gg1
}


