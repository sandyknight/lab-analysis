library(MatchIt)

dt <- readRDS("data/completed-data.rds")

m.out1 <-
  matchit(t ~
          age +
          year +
          currently_injecting +
          disabled_any +
          housing_problem +
          drug_benzodiazepine +
          utla23cd,
          data = dt,
          method = "nearest",
          distance = "glm")

m.data <- match_data(m.out1)

psm_fit0 <-
  glm(died ~ t,
      family = binomial(link = "logit"),
      data = m.data,
      weights = weights)

glm_fit0 <-
  glm(died ~ t +
      currently_injecting +
      age +
      disabled_any +
      housing_problem +
      drug_benzodiazepine,
      family = binomial(link = "logit"),
      data = dt)


stargazer::stargazer(psm_fit0, glm_fit0, apply.coef = exp, type = "text")


psm_fit1 <-
  lme4::glmer(died ~ t +
              age +
              currently_injecting +
              disabled_any +
              housing_problem +
              drug_benzodiazepine +
              (1 | utla23cd),
              lme4::glmerControl(optimizer = "bobyqa",
                           optCtrl =  list(maxfun = 1e05)),
              family = binomial(link = "logit"),
              weights = weights,
              data = m.data)


glm_fit1 <-
  lme4::glmer(died ~ t +
              age +
              currently_injecting +
              disabled_any +
              housing_problem +
              drug_benzodiazepine +
              (1 | utla23cd),
              lme4::glmerControl(optimizer = "bobyqa",
                           optCtrl =  list(maxfun = 1e05)),
              family = binomial(link = "logit"),
              data = m.data)

stargazer::stargazer(psm_fit1, glm_fit1, apply.coef = exp, type = "text")
