league_urls <- read_csv("league_urls.csv")

get_schedule_from_url <- function(my_url, rdsname, name) {
  Sys.sleep(1 + runif(1))
  h <- read_html(my_url)
  h %>% html_node(xpath = '//*[@id="site"]/div[2]/div[1]/div[1]/div[3]/div/table') -> hh
  if (class(hh) == "xml_missing") {
    # cat(str_c(fname, " missing.\n"))
    log_print(str_c(" * missing: ", name), console = FALSE)
    return(rdsname) # don't process and save
  } else {
    log_print(str_c(" * present: ", name), console = FALSE)
  }
  # check whether there is anything in it
  hh %>%
    html_table(header = FALSE) -> yyy
  if (nrow(yyy) == 0) {
    log_print(str_c(" * empty: ", name), console = FALSE)
    return(rdsname)
  }
  hh %>%
    html_table(header = FALSE) %>%
    filter(!str_detect(X1, "Round")) %>%
    mutate(X1 = ifelse(X1 == "", NA, X1)) %>%
    fill(X1) %>%
    mutate(X2 = ifelse(X2 == "", "00:00", X2)) %>%
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
    write_rds(paste0("rds/", rdsname)) -> x
  name
}

get_schedule_from_row <- function(the_row, the_leagues) {
  the_leagues %>% pluck("name", the_row) -> lname
  the_leagues %>% pluck("rds_name", the_row) -> lrdsname
  the_leagues %>% pluck("schedule_url", the_row) -> lschedurl
  log_print(lname, console = FALSE)
  get_schedule_from_url(lschedurl, lrdsname, lname)
}

download_these_ones <- function(l) {
  log_print(l$name)
  nr <- nrow(l)
  if (nr == 0) stop("no leagues to download")
  safely_get_schedule_row <- safely(get_schedule_from_row)
  walk(1:nr, \(i) safely_get_schedule_row(i, l), .progress = "lg dl")
}

display_the_league <- function(leagues, n) {
  if (nrow(leagues) == 0) stop ("no rows to display")
  leagues %>% pluck("rds_name", n) -> rdsname
  filename <- paste0("rds/", rdsname)
  if (file.exists(filename)) read_rds(filename) else list()
}

update_the_leagues <- function(leagues) {
  league_urls %>%
    mutate(r = row_number()) %>%
    rowwise() %>%
    mutate(mtime = file.mtime(paste0("rds/", rds_name))) %>%
    rowwise() %>%
    mutate(gg = list(display_the_league(league_urls, r))) %>%
    mutate(tibbly = is_tibble(gg)) %>%
    filter(tibbly) %>%
    unnest(gg) %>%
    filter(ko + hours(3) > mtime) %>%
    filter(ko < now()) -> gg1
  gg1 %>%
    count(name, rds_name, schedule_url) -> to_get
  if (nrow(to_get) == 0) stop("No leagues to get.")
  log_open("download.log")
  download_these_ones(to_get)
  log_close()
  gg1
}

show_the_changes <- function (ggg, league_urls) {
  ggg %>% count(r) %>% pull(r) -> r
  # league_urls %>% slice(r)
  map(r, \(r) display_the_league(league_urls, r)) %>% bind_rows() -> all_games
  ggg %>% left_join(all_games, by = c("t1", "t2")) %>%
    mutate(show = case_when(
      is.na(score.y) & !is.na(score.x) ~ "no",
      is.na(score.y) ~ "yes",
      is.na(score.x) ~ "yes",
      score.x == score.y ~ "no",
      .default = "yes"
    )) %>%
    filter(show == "yes") %>%
    select(-tibbly) %>%
    select(name, ko = ko.y, t1, t2, score.x, score.y) %>%
    arrange(ko)
}
