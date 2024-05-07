#!/usr/bin/env Rscript

# Load packages & custom functions —————————————————————————————————————————————————————————————————————————————————————
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  # data manipulation & analysis
  readxl, tidyverse, 
  # file management
  here, fs,
  # base types
  rlang 
  )
source(here("R/functions.R"))

# 01: Process Meta-Analysis/Multisite-Replication Data  ————————————————————————————————————————————————————————————————

cli::cli_alert_info("Processing Meta-Analysis/Multisite-Replication Data")
path <- here("data/meta/raw/data_lindenhonekopp_raw.xlsx")
data_raw <- map(
  .x = set_names(excel_sheets(path)), 
  .f = ~read_excel(path, sheet = .x, progress = FALSE)
) |> 
  suppressMessages() |>
  bind_rows(.id = "id_meta") |> 
  select(id_meta, n = N, d = d_obs) |> 
  mutate(id_meta = tolower(str_replace_all(id_meta, " ", "_")))

metadata <- read_csv(here("data/meta/raw/metadata_lindenhonekopp.csv"))
data_comb <- inner_join(data_raw, metadata, by = join_by(id_meta))  

non_integer_n <- which(!sapply(data_comb$n, is_integerish))
data_comb$n <- ceiling(data_comb$n)

# Save data
meta_file <- "data/meta/processed/data_lindenhonekopp_proc.csv"
cli::cli_alert_info("Saving processed meta-analytical data to {.file {meta_file}}")
write_csv(
  x = data_comb, 
  file = here(meta_file)
  )

# 02: Process DE-Optimization Data (SPEEC-Approach)  ————————————————————————————————————————————————————————————————

cli::cli_alert_info("Processing Optimization Data from SPEEC")

# Load in optimization data
optim_files <- dir_ls(here("data/optim/raw"), regexp = "\\.rds$")
optim_names <- gsub("\\.rds$", "", basename(optim_files))
optim_list <- map(optim_files, read_rds)
names(optim_list) <- gsub("^optim_de", "", gsub("\\.rds$", "", basename(optim_files)))


# Write Parameters Data
file_optim_par <- "data/optim/processed/data_optim_merged.csv"
cli::cli_alert_info("Saving processed parameter optimization data to {.file {file_optim_par}}")
data_par <- extract_optim(optim_list) |> 
  mutate(
    w_pbs = smithson_verkuilen(w_pbs),
    runtime = as.numeric(runtime, units = "secs")
    )
write_csv(data_par, here(file_optim_par))

file_optim_evolution <- "data/optim/processed/data_optim_evolution.csv.gz"
cli::cli_alert_info("Saving processed parameter evolution data to {.file {file_optim_evolution}}")
data_evolution <- extract_evolution(optim_list)
write_csv(data_evolution, gzfile(here(file_optim_evolution)))


file_optim_loss <- "data/optim/processed/data_optim_loss.csv.gz"
cli::cli_alert_info("Saving processed parameter loss data to {.file {file_optim_loss}}")
data_loss <- extract_loss(optim_list)
write_csv(data_loss, gzfile(here(file_optim_loss)))

file_optim_confidence <- "data/optim/processed/data_optim_evolution_confidence.csv.gz"
cli::cli_alert_info("Saving processed parameter confidence data to {.file {file_optim_confidence}}")
data_confidence <- data_evolution |>
  pivot_longer(
    cols = -c(id_meta, id_iter),
    names_to = "parameter",
    values_to = "value"
  ) |>
  group_by(id_meta, parameter, id_iter) |>
  summarise(
    mean = mean(value),
    ub = mean + qnorm(0.975) * sd(value) / sqrt(n()),
    lb = mean - qnorm(0.975) * sd(value) / sqrt(n())
  ) |>
  ungroup()
write_csv(x = data_confidence, gzfile(here(file_optim_confidence)))