
library(tidyverse)
library(rvest)
library(stringdist) # idea from https://www.r-bloggers.com/2017/12/vectorize-fuzzy-matching/
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(tidyr::extract)
source("functions.R")
source("rate_functions.R")
source("stats_functions.R")
source("predictions_functions.R")



the_url <- "https://www.predictthefootball.com/?lang=en_us"
sess <- session(the_url)
form <- html_form(sess)
# form
filled_form <- html_form_set(form[[1]],
                          "LoginForm[email]" = Sys.getenv("PTF_USER"),
                          "LoginForm[password]" = Sys.getenv("PTF_PASS"),
                          "LoginForm[rememberMe]" = 1
                          )

session_submit(sess, filled_form)



leagues_txt <- "
id, lg, rdsname, ptf
1, eng-ch, eng_championship_2023-2024.rds, https://championship.predictthefootball.com
2, eng-pr, eng_premier-league_2023-2024.rds, https://premierleague.predictthefootball.com
3, fra, fra_ligue-1_2023-2024.rds, https://ligue1.predictthefootball.com
4, ger, ger_bundesliga_2023-2024.rds, https://bundesliga.predictthefootball.com
5, gre, gre_super-league_2023-2024.rds, https://superleague.predictthefootball.com
6, ita, ita_serie-a_2023-2024.rds, https://seriea.predictthefootball.com
7, sco, sco_premiership_2023-2024.rds, https://scottish-premier.predictthefootball.com
8, esp, esp_primera-division_2023-2024.rds, https://laliga.predictthefootball.com
9, swe, swe_allsvenskan_2024.rds, https://allsvenskan.predictthefootball.com
"

league_df <- read_csv(leagues_txt)

# remove greece for the moment

# 5a, gre1, gre_super-league_2023-2024_meisterschaft.rds, https://superleague.predictthefootball.com
# 5b, gre2, gre,super-league_2023-2024_abstieg.rds, https://superleague.predictthefootball.com




get_league_preds <- function(league_url, fname, sess) {
  lu_name <- str_c("lu/", fname)
  lu <- read_rds(lu_name)
  get_form(league_url, sess) %>%
    filter(is.na(pred_1)) -> ptf_form
  ptf_form %>%
    mutate(t1 = best_match(name_1, lu),
           t2 = best_match(name_2, lu)) %>%
    left_join(lu, by = c("t1" = "team")) %>%
    left_join(lu, by = c("t2" = "team")) %>%  # id.x and id.y are the teams to look up
    rowwise() %>%
    mutate(ppd = list(make_ppd(fname, id.x, id.y))) %>%
    mutate(opt_pred = max_ept(ppd, results, scores)) %>%
    select(name_1, name_2, opt_pred)
}



suppressWarnings(
  league_df %>%
    # slice(1:3) %>%
    rowwise() %>%
    mutate(preds = list(get_league_preds(ptf, rdsname, sess))) %>%
    unnest(preds) %>%
    select(lg, name_1, name_2, opt_pred) -> d
)
d %>%
  arrange(lg) %>%
  knitr::kable()



get_form("https://seriea.predictthefootball.com", sess) %>%
    filter(is.na(pred_1)) -> ptf_form
fname <- "ita_serie-a_2023-2024.rds"
lu_name <- str_c("lu/", fname)
lu <- read_rds(lu_name)
options("dplyr.summarise.inform" = FALSE)
suppressWarnings(
  ptf_form %>%
    mutate(t1 = best_match(name_1, lu),
           t2 = best_match(name_2, lu)) %>%
    select(name_1, name_2, t1, t2) %>%
    left_join(lu, join_by("t1" == "team")) %>%
    left_join(lu, join_by("t2" == "team")) %>%
    rowwise() %>%
    mutate(ppd = list(make_ppd(fname, id.x, id.y))) %>%
    unnest(ppd) %>%
    select(name_1, name_2, score, p) %>%
    separate_wider_delim(score, delim = "-", names = c("s1", "s2")) %>%
    mutate(across(starts_with("s"), \(x) as.numeric(x))) %>%
    group_by(name_1, name_2) %>%
    summarize(s1_mean = sum(s1*p), s2_mean = sum(s2*p)) %>%
    mutate(total = s1_mean + s2_mean, diff = s1_mean - s2_mean) %>%
    knitr::kable()

)



