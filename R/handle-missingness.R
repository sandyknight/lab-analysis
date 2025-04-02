library(data.table)
library(mice)
library(furrr)

set.seed(910)

dt <- readr::read_rds("data/glmer-dataframe.rds")

dt <- data.table::as.data.table(dt)

dt[, .N, by = .(currently_injecting)] |>
  dplyr::mutate(p = scales::percent(N / sum(N)))

dt[, .N, by = .(disabled_any)] |>
  dplyr::mutate(p = scales::percent(N / sum(N)))

dt[, .N, by = .(housing_problem)] |>
  dplyr::mutate(p = scales::percent(N / sum(N)))

imp <- mice::futuremice(dt, m = 20, maxit = 5, n.core = 2)

saveRDS(imp, "data/imputations.rds")

mdf <- mice::complete(imp)

mdf <- data.table::as.data.table(mdf)

data.table::fwrite(mdf, "data/completed-data.csv")
