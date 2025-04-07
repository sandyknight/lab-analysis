library(data.table)
library(dtplyr)
library(dplyr)
# Load SIR table
sir_dt <-
  data.table::fread("data/SIR_table_for_VfM_linked.csv")

# Select cols
sir_dt <-
  sir_dt[, .(client_random_id, n_jy, submoddt, phbudi_any)]

# Load main table
main_dt <-
  data.table::fread("data/K3anon_FullDataset_for_VfM.csv")


# Select cols and change discharge date to last SIR if NA
main_dt <-
  main_dt[, .(
    client_random_id,
    n_jy,
    triaged,
    disrsn,
    disd,
    utla23cd,
    sex,
    age,
    ethnic,
    sexualo,
    disable,
    housing_start,
    homeless_start,
    drug_alcohol,
    drug_heroin,
    drug_crack,
    drug_cocaine,
    drug_benzodiazepine,
    injstat
  )]

# Merge tables
dt <-
  data.table::merge.data.table(
    main_dt,
    sir_dt,
    by = c("client_random_id", "n_jy")
  )

# Assume exit reason inconsistent mean still in treatment
# if discharge date is cutoff (was NA)
dt[,
  disrsn := data.table::fifelse(
    disrsn == "Exit reason inconsistent" &
      is.na(disd),
    "Continued in treatment",
    disrsn
  )
]

dt[, disd := fifelse(is.na(disd), submoddt, disd)]

# Calculate days between discharge or cut-off and SIR
dt[, days_since_sir := disd - submoddt]


# Intervention (binary) --------------------------------------------------------

# The intervention (t) is: 'Recieved depot buprenorphine in the 12 months
# preceding discharge or, if still in treatment, last recorded SIR.'

# Assign to treatment group 1 if depot bupreonorophine was
# indicated at a SIR in the 12 months before discharge or last SIR

dt[, t := fifelse(phbudi_any == 1 & days_since_sir < 366, 1, 0)]

dt[, t := factor(t, levels = 0:1)]

# Outcome (binary) -------------------------------------------------------------

# Outcome of interest is mortality, if discharge reason is recorded as "Died",
# outcome variable `died` = 1, any other discharge reason, or continued
# treatment, `died` = 0

dt <-
  dt |>
  dtplyr::lazy_dt() |>
  mutate(died = if_else(disrsn == "Died", 1, 0)) |>
  mutate(
    successful_completion = if_else(disrsn == "Successful completion", 1, 0)
  ) |>
  mutate(
    retention = if_else(
      disrsn %in%
        c("Exit reason inconsistent", "", "Incomplete other") &
        (disd - triaged) > 84,
      1,
      0
    )
  )

dt <- data.table::as.data.table(dt)

# Covariates -------------------------------------------------------------------

# 1. Year (categorical, 3)

dt[, year := lubridate::year(disd)]

dt <- dt[year > 2021 & year < 2025, ]

dt[, year := factor(year, levels = c(2024, 2023, 2022))]

# 2. Age group (categorical, 11)

dt[, age := factor(age, levels = sort(unique(dt[["age"]])), ordered = FALSE)]

# 3. Currently injecting (binary)

dt <- dt[injstat %notin% c("Missing or inconsistent", "Declined to answer"), ]

dt[,
  currently_injecting := factor(
    injstat,
    levels = c("Never injected", "Previously injected", "Currently injecting")
  )
]

# 4. Upper-tier local authority (cluster, random intercepts, 152)

utla_check <- dt[, .N, by = "utla23cd"]

setorder(utla_check, N)

exc_utlas <-
  utla_check[1:2, ][["utla23cd"]]

dt <- dt[utla23cd %notin% exc_utlas, ]

# 5. Any disability

dt[, disabled_any := fifelse(disable == "Any disability", 1, 0)]

# 6. Any housing problem

dt <- dt[housing_start != "Missing/inconsistent", ]

dt[,
  housing_problem := fifelse(
    housing_start %in% c("No fixed abode", "Rough sleeping"),
    1,
    0
  )
]

# 7. Drug use at SIR

# Model dataset ---------------------------------------------------------------

dt <-
  dt[, .(
    client_random_id,
    n_jy,
    died,
    successful_completion,
    retention,
    t,
    year,
    sex,
    age,
    currently_injecting,
    disabled_any,
    housing_problem,
    drug_alcohol,
    drug_heroin,
    drug_crack,
    drug_cocaine,
    drug_benzodiazepine,
    utla23cd
  )]

dt <- unique(dt)


dt[, .(year)]

# Save data

data.table::fwrite(dt, "data/glmer-dataframe.csv")
