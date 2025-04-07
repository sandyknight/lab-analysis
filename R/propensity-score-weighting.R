library(data.table)
library(marginaleffects)
library(WeightIt)

dt <- data.table::fread("data/glmer-dataframe.csv")

W <-
  WeightIt::weightit(
    died ~
      t +
        age +
        utla23cd +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine,
    data = dt,
    method = "glm",
    estimand = "ATT"
  )

cobalt::bal.tab(W, un = TRUE)

summary(W)

#Logistic regression model with covariates

fit_psw <- WeightIt::glm_weightit(
  died ~
    t +
      age +
      utla23cd +
      currently_injecting +
      disabled_any +
      housing_problem +
      drug_benzodiazepine,
  data = dt,
  weightit = W,
  family = "binomial"
)

summary(fit_psw)


stargazer::stargazer(fit_psw, type = "text", apply.coef = exp)


dt$ps_weights <- W$weights

re_logisitc_fit2 <-
  lme4::glmer(
    died ~
      t +
        age +
        currently_injecting +
        disabled_any +
        housing_problem +
        drug_benzodiazepine +
        (1 | utla23cd),
    family = binomial(link = "logit"),
    data = dt,
    weights = ps_weights
  )

summary(re_logisitc_fit2)

ps_weights <- dt[["ps_weights"]]

weighted_ones <- sum(dt$died * dt$ps_weights)
weighted_zeros <- sum((1 - dt$died) * dt$ps_weights)

ps_weights <- order(ps_weights)

library(dplyr)
dt %>%
  group_by(utla23cd) %>%
  summarise(
    weighted_ones = sum(died * ps_weights),
    weighted_zeros = sum((1 - died) * ps_weights),
    .groups = "drop"
  ) %>%
  mutate(
    total = weighted_ones + weighted_zeros,
    fraction_died = weighted_ones / total
  ) %>%
  arrange(fraction_died)

dt %>%
  group_by(utla23cd) %>%
  summarise(
    weighted_ones = sum(died * ps_weights),
    weighted_zeros = sum((1 - died) * ps_weights),
    .groups = "drop"
  ) %>%
  mutate(
    total = weighted_ones + weighted_zeros,
    fraction_died = weighted_ones / total
  ) %>%
  arrange(desc(fraction_died))


df_loc <- dt %>%
  group_by(utla23cd) %>%
  summarise(
    weighted_ones = sum(died * ps_weights),
    weighted_zeros = sum((1 - died) * ps_weights),
    total = weighted_ones + weighted_zeros,
    fraction_died = weighted_ones / total,
    .groups = "drop"
  )

summary(df_loc$fraction_died)

summary(df_loc$total)
