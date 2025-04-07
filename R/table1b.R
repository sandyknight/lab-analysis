
library(data.table)
library(dplyr)


dt <- data.table::fread("data/glmer-dataframe.csv")

d1 <-
dt |>
  group_by(t, died) |>
  tally()

d2 <-
dt |>
  group_by(t, successful_completion) |>
  tally()


d3 <-
  dt |>
    group_by(t, retention) |>
    tally()
