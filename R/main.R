library(data.table)
library(lme4)

# Load SIR table
sir_dt  <-
  data.table::fread("data/SIR_table_for_VfM_linked.csv")

# Select cols
sir_dt <-
  sir_dt[, .(client_random_id,
             n_jy,
             submoddt,
             phbudi_any)]

# Load main table
main_dt <-
  data.table::fread("data/K3anon_FullDataset_for_VfM.csv")


# Select cols and change discharge date to cut-off date if NA
main_dt <-
  main_dt[, .(client_random_id,
              n_jy,
              disd,
              disrsn,
              age,
              utla23cd,
              injstat)]

# Merge tables
dt <-
  data.table::merge.data.table(main_dt,
                               sir_dt,
                               by = c("client_random_id", "n_jy"))

# Assume exit reason inconsistent mean still in treatment
# if discharge date is cutoff (was NA)
dt[, disrsn := data.table::fifelse(disrsn == "Exit reason inconsistent" &
                                     is.na(disd),
                                   "Continued in treatment", disrsn)]

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

dt[, died := fifelse(disrsn == "Died", 1, 0)]

dt[, died := factor(died, levels = 0:1)]


# Covariates -------------------------------------------------------------------

# 1. Year (categorical, 3)

dt[, year := lubridate::year(disd)]

dt <- dt[year > 2021 & year < 2025, ]

dt[, year := factor(year, levels = 2022:2024)]

# 2. Age group (categorical, 11)

dt[, age := factor(age, levels = sort(unique(dt[["age"]])), ordered = TRUE)]

# 3. Currently injecting (binary)

dt[, currently_injecting := fifelse(injstat == "Currently injecting", 1, 0)]

# 4. Upper-tier local authority (cluster, random intercepts, 152)

dt[, utla23cd := factor(utla23cd, ordered = FALSE)]


# Model dataset ---------------------------------------------------------------

dt <-
  dt[, .(client_random_id,
         n_jy,
         year,
         age,
         currently_injecting,
         utla23cd,
         t,
         died)]

dt <- unique(dt)

# Model fitting ---------------------------------------------------------------

time0 <- Sys.time()

re_logisitc_fit1 <-
  lme4::glmer(died ~ t + year + age + currently_injecting + (1 | utla23cd),
              family = binomial(link = "logit"),
              data = dt)

time1 <- Sys.time()

timefit1 <- time1 - time0

readr::write_rds(re_logisitc_fit1, "fits/random-effects-logistic-fit1.rds")

print(timefit1)
