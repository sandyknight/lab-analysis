library(data.table)


dt <- fread("data/glmer-dataframe.csv")


injstat_summary <- dt[, .N, by = c("currently_injecting")]

injstat_summary |>
  janitor::adorn_totals() |>
  janitor::adorn_percentages()
