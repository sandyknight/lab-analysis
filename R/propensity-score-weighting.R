library(data.table)
library(marginaleffects)
library(WeightIt)

dt <- readRDS("data/completed-data.rds")

W <-
  WeightIt::weightit(t ~
                     age +
                     utla23cd +
                     currently_injecting +
                     disabled_any +
                     housing_problem +
                     drug_benzodiazepine,
                     data = dt, method = "glm",
                     estimand = "ATT")

#cobalt::bal.tab(W, un = TRUE)

#summary(W)

#Logistic regression model with covariates

dt$ps_weights <- W$weights

psw_fit0 <-
  glm(died ~ t +
      currently_injecting +
      age +
      disabled_any +
      housing_problem +
      drug_benzodiazepine,
      family = binomial(link = "logit"),
      data = dt)

psw_fit1 <-
  lme4::glmer(died ~ t +
              age +
              currently_injecting +
              disabled_any +
              housing_problem +
              drug_benzodiazepine +
              (1 | utla23cd),
              family = binomial(link = "logit"),
              data = dt, weights =  ps_weights)

stargazer::stargazer(psw_fit0, psw_fit1, type = "text", apply.coef = exp)
