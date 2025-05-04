get_stats <- function(my_url, session) {
  page <- session_jump_to(session, my_url)
  page %>% read_html()  %>%
    html_text() %>% str_split("\n\t") %>%
    .[[1]] %>%
    str_replace_all("[\n\t ]", "") -> v
  if (length(v)<9) {
    return(list())
  }
  # number of predictions
  v[1] %>%
    str_extract("(?<=:).*$") %>% as.numeric() -> n_pred
  # result stats
  v[c(2,4,6)] %>% str_extract("[0-9.]+") %>% as.numeric() -> results
  results + 100/(n_pred-1) -> w
  names(w) <- 2:0
  enframe(w) %>% filter(value<19.5) %>% pull(name) %>%
    paste(collapse = "") -> result_stats
  if (result_stats == "") result_stats <- "X"
  # score_stats
  v[-c(1:9, length(v))] %>% enframe() %>%
    mutate(odd = name %% 2) %>%
    mutate(div = name %/% 2 + odd) %>%
    select(-name) %>%
    pivot_wider(names_from = odd, values_from = value) %>%
    rename("score" = `1`, percent = `0`) %>%
    mutate(percent2 = str_extract(percent, "[0-9.]+")) %>%
    mutate(percent3 = as.numeric(percent2)) %>%
    mutate(percent4 = percent3 + 100/(n_pred-1)) %>%
    filter(percent4 > 4.5) %>%
    pull(score) %>%
    paste(collapse = ":") -> score_stats
  list(preds = n_pred, results = result_stats, scores = score_stats)
}

get_form_html <- function(base_url, session) {
  print(base_url)
  index_page <- "/profile/index"
  my_url <- str_c(base_url, index_page)
  page <- session_jump_to(session, my_url)
  page %>% read_html()
}

get_form_itself <- function(html_code, base_url) {
  html_code %>%   html_nodes("form") %>%
    .[[2]] %>% html_nodes("td") -> as_form
  as_form %>% html_attr("class") -> classes
  as_form %>% html_text() -> texts
  as_form %>% html_nodes("a") %>%
    html_attr("href") -> fixture_hrefs
  tibble(class=classes, text = texts) %>%
    filter(str_detect(class, "^team-name")) %>%
    mutate(col = gl(n = 2, k = 1, length = nrow(.))) %>%
    mutate(row = gl(n = nrow(.)/2, k=2)) %>%
    select(-class) %>%
    extract(text, into = "pred", regex = "\t +([0-9]+)\t", remove = FALSE) %>%
    extract(text, into = "name1", regex = "[0-9]+\t +(.*)\n\t *$", remove = FALSE) %>%
    extract(text, into = "name2", regex = "\n\t +(.*)\n\t *$") %>%
    mutate(name = ifelse(is.na(pred), name2, name1)) %>%
    select(-name1, -name2) %>%
    pivot_wider(names_from = col, values_from = c(name, pred)) %>%
    mutate(stats_url = str_c(base_url, fixture_hrefs)) %>%
    extract(stats_url, into = "game_number", regex = "fixtureid=([0-9]+)$", remove = FALSE) %>%
    mutate(stats = map(stats_url, ~get_stats(.))) %>%
    unnest_wider(stats) %>%
    select(-row, -stats_url)
}



get_form <- function(base_url, session) {
  print(base_url)
  index_page <- "/profile/index"
  my_url <- str_c(base_url, index_page)
  page <- session_jump_to(session, my_url)
  page %>% read_html() %>%   html_nodes("form") %>%
    .[[2]] %>% html_nodes("td") -> as_form
  as_form %>% html_attr("class") -> classes
  as_form %>% html_text() -> texts
  as_form %>% html_nodes("a") %>%
    html_attr("href") -> fixture_hrefs
  tibble(class=classes, text = texts) %>%
    filter(str_detect(class, "^team-name")) %>%
    mutate(col = gl(n = 2, k = 1, length = nrow(.))) %>%
    mutate(row = gl(n = nrow(.)/2, k=2)) %>%
    select(-class) %>%
    extract(text, into = "pred", regex = "\t +([0-9]+)\t", remove = FALSE) %>%
    extract(text, into = "name1", regex = "[0-9]+\t +(.*)\n\t *$", remove = FALSE) %>%
    extract(text, into = "name2", regex = "\n\t +(.*)\n\t *$") %>%
    mutate(name = ifelse(is.na(pred), name2, name1)) %>%
    select(-name1, -name2) %>%
    pivot_wider(names_from = col, values_from = c(name, pred)) %>%
    mutate(stats_url = str_c(base_url, fixture_hrefs)) %>%
    extract(stats_url, into = "game_number", regex = "fixtureid=([0-9]+)$", remove = FALSE) %>%
    mutate(stats = map(stats_url, ~get_stats(., session))) %>%
    unnest_wider(stats) %>%
    select(-row, -stats_url)
}

