using CSV, DataFrames, MixedModels

dt = DataFrame(CSV.File("data/glmer-dataframe.csv"))

model = fit(MixedModel,
            @formula(died ~ t + year + age + currently_injecting + disabled_any +
                    housing_problem + drug_alcohol + drug_heroin + drug_crack +
                    drug_cocaine + drug_benzodiazepine + (1 | utla23cd)),
            dt,
            Bernoulli(),
            LogitLink()
           )

