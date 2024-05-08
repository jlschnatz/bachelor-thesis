#!/usr/bin/env Rscript


pacman::p_load(tidyverse, here, betareg, broom, insight)
source(here("R/functions.R"))
data_parameter <- read_csv(here("data/optim/processed/data_optim_merged.csv"))

# Intercept Model 
mod <- betareg(
    formula = w_pbs ~ 1 | 1,
    data = data_parameter,
    link = "logit",
    link.phi = "identity"
    )

summary(mod)
write_rds(mod, here("data/src/model_dispersion.rds"))

table_intercept <- tidy(mod, conf.int = TRUE) |>
    mutate(ci = format_ci(conf.low, conf.high, ci = NULL)) |>
    mutate(prob = if_else(component == "mean", format_value(plogis(estimate)), ""), .after = estimate) |>
    mutate(across(c(estimate, std.error, statistic), format_value)) |>
    mutate(p.value = format_p(p.value, name = NULL)) |>
    mutate(estimate = if_else(component == "mean", str_c("$",estimate, "^a$"), str_c("$", estimate, "^b$"))) |>
    select(-starts_with("conf"), -c(component, term)) |>
    nice_table(
        caption = "Intercept Beta Regression Model Comprising the Full Dataset",
        col_names = c("Estimate", "Probability", "$SE$", "$z$", "$p$", "$CI$"),
        general_fn = "SE: standard error; CI: 95\\\\% confidence interval",
        alphabet_fn = c("$\\\\log$($OR$)", "Identity")
    ) |>
    kableExtra::group_rows("Mean Component Intercept $\\mu$", 1, 1, escape = FALSE) |>
    kableExtra::group_rows("Precision Component Intercept $\\phi$", 2, 2, escape = FALSE) 

cat(table_intercept, file = here("tables/intercept_table.tex"))


