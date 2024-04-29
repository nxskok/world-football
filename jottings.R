rdsname <- "wal_premier-league_2023-2024_relegation.rds"
rdsname <- "jpn_j1-league_2024.rds"
rdsname <- "bra_serie-a_2024.rds"
next_rate_date(rdsname)
first_dup
# first_dup_copy <- first_dup
first_dup <- first_dup_copy
hour(first_dup)


if (hour(first_dup) > 17) {
  hour(first_dup) <- 17
  minute(first_dup) <- 0
} else {
  hour(first_dup) <- 17
  minute(first_dup) <- 0
  first_dup <- first_dup - days(1)
}
first_dup
