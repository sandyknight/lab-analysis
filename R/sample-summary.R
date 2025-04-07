library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

dt <- data.table::fread("data/glmer-dataframe.csv")


calculate_sample_summary <-
  function(data) {
    df <-
      data |>
      dtplyr::lazy_dt() |>
      mutate(
        disabled_any = if_else(disabled_any == 0, "No disability", "Any disability")
      ) |>
      mutate(
        housing_problem = if_else(
          housing_problem == 0,
          "No housing problem",
          "Any housing problem"
        )
      ) |>
      group_by(t, sex, age, currently_injecting, disabled_any, housing_problem) |>
      tally() |>
      tibble::as_tibble()


    df <-
    df |>
      pivot_longer(cols = sex:housing_problem, names_to = "characteristic") |>
      pivot_wider(names_from = t, values_from = n, values_fn = sum) |>
      mutate(
        characteristic = factor(
          characteristic,
          levels = c(
            "sex",
            "age",
            "currently_injecting",
            "disabled_any",
            "housing_problem"
          )
        )
      ) |>
      arrange(characteristic)

    colnames(df) <- c("Characteristic", "charval", "Control", "Treated")

    return(df)

  }

table1 <- left_join(calculate_sample_summary(dt),
          calculate_sample_summary(m.data),
          by = c("Characteristic", "charval"),
          suffix =  c("_unmatched_n", "_matched_n"))

t1 <-
dt |>
  group_by(t) |>
  tally() |>
  mutate(psm = "Unmatched")

t2 <-
m.data |>
  group_by(t) |>
  tally() |>
  mutate(psm = "Matched")

t0 <- rbind(t1, t2)


table1 <-
table1 |>
  mutate(Characteristic = snakecase::to_sentence_case(as.character(Characteristic))) |>
  mutate(Characteristic = paste(Characteristic, charval, sep = ": ")) |>
  select(-charval)

table1_formatted <-
table1 |>
  mutate(control_unmatched_p = Control_unmatched_n / as.integer(t0[1, "n"])) |>
  mutate(treated_unmatched_p = Treated_unmatched_n / as.integer(t0[2, "n"])) |>
  mutate(control_matched_p = Control_unmatched_n / as.integer(t0[3, "n"])) |>
  mutate(treated_matched_p = Treated_unmatched_n / as.integer(t0[4, "n"])) |>
  select(Characteristic,
         Control_unmatched_n,
         control_unmatched_p,
         Treated_unmatched_n,
         treated_unmatched_p,
         Control_matched_n,
         control_matched_p,
         Treated_matched_n,
         treated_matched_p) |>
  mutate(across(contains("_p"), scales::percent)) |>
  mutate(across(contains("_n"), scales::comma))

table1_formatted |>
  select(treated_matched_p)

t0
