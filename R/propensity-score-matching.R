library(MatchIt)
library(data.table)
library(dplyr)


dt <- data.table::fread("data/glmer-dataframe.csv")

m.out1 <-
MatchIt::matchit(
    t ~
        age +
        utla23cd +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine,
    data = dt,
    method = "nearest")

m.data <- match_data(m.out1)

fit <-
glm(died ~
    t,
    family = binomial(link = "logit"),
    data = m.data)

fit_confints <- confint(fit)

table2_row_psm <-
broom::tidy(fit) |>
  mutate(OR = exp(estimate)) |>
  filter(term == "t") |>
  mutate(Outcome = "Mortality") |>
  bind_cols(tibble::as_tibble_row(exp(fit_confints["t", ]))) |>
  select(Outcome, OR, `2.5 %`, `97.5 %`, p.value) |>
  mutate(Model = "Logistic regression with PSM")


fit0 <-
glm(died ~
    t +
        age +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine,
    family = binomial(link = "logit"),
    data = dt)


fit0_confints <- confint(fit0)

table2_row_unmatched_adjusted <-
  broom::tidy(fit0) |>
    mutate(OR = exp(estimate)) |>
    filter(term == "t") |>
    mutate(Outcome = "Mortality") |>
    bind_cols(tibble::as_tibble_row(exp(fit0_confints["t", ]))) |>
    select(Outcome, OR, `2.5 %`, `97.5 %`, p.value) |>
    mutate(Model = "Logistic regression with fixed effects")


fit <-
  glm(died ~
      t,
      family = binomial(link = "logit"),
      data = m.data)



fitsc <-
  glm(successful_completion ~
      t,
      family = binomial(link = "logit"),
      data = m.data)

fitsc_confints <- confint(fitsc)

table2_row_sc_psm <-
  broom::tidy(fitsc) |>
    mutate(OR = exp(estimate)) |>
    filter(term == "t") |>
    mutate(Outcome = "Successful completion") |>
    bind_cols(tibble::as_tibble_row(exp(fitsc_confints["t", ]))) |>
    select(Outcome, OR, `2.5 %`, `97.5 %`, p.value) |>
    mutate(Model = "Logistic regression with PSM")


table2 <- bind_rows(list(table2_row_unmatched_adjusted,
                         table2_row_psm,
                         table2_row_sc_psm))

table2

table2 |>
  flextable::flextable()
