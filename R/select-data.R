library(data.table)
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

dt <-
  data.table::merge.data.table(
    main_dt,
    sir_dt,
    by = c("client_random_id", "n_jy")
  )

dt[, disd := fifelse(is.na(disd), submoddt, disd)]

dt[, year := lubridate::year(disd)]

dt <- dt[year > 2021 & year < 2025, ]

data.table::fwrite(dt, "data/selected-data.csv")
