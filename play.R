
library(tidyverse)
library(rvest)
library(conflicted)
conflicts_prefer(dplyr::filter)
source("functions.R")
source("predictions_functions.R")

#' download leagues from just this country

sch <- c("blr")
leagues %>% mutate(r = row_number()) %>%  filter(country %in% sch) -> dr
dr


#| warning: false

these <- dr$r
leagues %>% slice(these) %>% download_these()
right_now <- now()
enframe(these, name = NULL) %>%
  rowwise() %>%
  mutate(lg = list(display_league(leagues, value))) %>%
  unnest(lg) %>%
  mutate(days = time_length(right_now - ko, unit = "days")) %>%
  filter(between(days, -3, 7)) %>%
  View("games downloaded")

update_predictions(leagues)
