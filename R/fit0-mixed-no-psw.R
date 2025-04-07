library(data.table)
library(lme4)

data.table::fread("data/glmer-dataframe.csv")

dt[, .(year)]

logistic_fit0 <-
  glm(
    died ~
      t +
        age +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine +
        utla23cd,
    family = binomial(link = "logit"),
    data = dt
  )


summary(logistic_fit0)

library(stargazer)

stargazer(logistic_fit0, apply.coef = exp, type = "text")

dt[, .(year)]

# Random effects at utla23cd ---------------------------------------------------

re_logisitc_fit1 <-
  lme4::glmer(
    died ~
      t +
        age +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine +
        (1 | utla23cd),
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e05)),
    family = binomial(link = "logit"),
    data = dt
  )


stargazer(re_logisitc_fit1, apply.coef = exp, type = "text")

summary(re_logisitc_fit1)


dt[, .N, by = c("year", "died", "t")] |>
  tibble::as_tibble()
