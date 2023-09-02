library(tidyverse)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Supply some inputs.")
args_num <- round(as.numeric(args) * 60)
print(args_num)
now <- now()
read_rds("predictions.rds") %>%
  filter(between(ko, now - minutes(args_num[1]),
                 now + minutes(args_num[2]))) %>%
  arrange(fname) %>%
  knitr::kable()
