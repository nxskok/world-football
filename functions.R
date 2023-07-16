# functions for worldfootball.net

make_fname <- function(x, y, z) {
  str_c("rds/", x, "_", y, "_", z, ".rds")
}


get_schedule <- function(country_name, league_name, season) {
  Sys.sleep(1)
  my_url <- str_c("https://www.worldfootball.net/all_matches/", country_name, "-", league_name, "-", season, "/")
  fname <- make_fname(country_name, league_name, season)
  h <- read_html(my_url)
  h %>% html_node(xpath = '//*[@id="site"]/div[2]/div[1]/div[1]/div[3]/div/table') %>%
    html_table(header = FALSE) %>%
    filter(!str_detect(X1, "Round")) %>%
    mutate(X1 = ifelse(X1 == "", NA, X1)) %>%
    fill(X1) %>%
    mutate(kostr = str_c(X1, " ", X2)) %>%
    mutate(ko = dmy_hm(kostr, tz = "Europe/London")) %>%  # it looks as if it's UK time
    mutate(ko = with_tz(ko, "America/Toronto")) %>%
    select(ko, t1 = X3, t2 = X5, score0 = X6) %>%
    extract(score0, into = c("s1", "s2"), "^(\\d+):(\\d+)") %>%
    mutate(score = str_c(s1, " - ", s2)) %>%
    select(-s1, -s2) %>%
    write_rds(fname) -> x
  fname
}


leagues <- read_csv("leagues.csv")


get_schedule_row <- function(n, leagues) {
  with(leagues, get_schedule(country[n], league[n], season[n]))
}


display_league <- function(n, leagues) {
  leagues %>% slice(n) %>%
    mutate(fname = make_fname(country, league, season)) %>%
    pull(fname) -> filename
  read_rds(filename)
}

find_games <- function(leagues, datum_time = now(), hours_back = 0, hours_forward = 0) {
  leagues %>% mutate(r = row_number()) %>%
    rowwise() %>%
    mutate(games = list(display_league(r, leagues))) %>%
    unnest(games) %>%
    mutate(hours = as.period(ko %--% datum_time) / hours(1)) %>%
    filter(hours <= hours_back) %>%
    filter(-hours <= hours_forward)
}


