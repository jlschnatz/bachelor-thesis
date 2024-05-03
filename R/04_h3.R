# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  tidyverse,          # data manipulation
  sysfonts, showtext, # fonts
  here,               # file management
  latex2exp,          # latex formula
  kableExtra,         # tables
  TOSTER, insight     # modeling
  )
source(here("R/00_functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
font_add_google("Noto Sans Math", "font")
showtext_opts(dpi = 500)
showtext_auto()

# Load data ————————————————————————————————————————————————————————————————————————————————————————————————————————————
data_meta <- read_csv(here("data/meta/processed/data_lindenhonekopp_proc.csv"))
data_optim <- read_csv(here("data/optim/processed/data_optim_merged.csv"))

# TOSTER Equivalence Test for Dependent Means ——————————————————————————————————————————————————————————————————————————
data_model <- data_meta |> 
  filter(type_synthesis == "mr") |> 
  group_by(id_meta) |> 
  summarise(mean_d = mean(d)) |> 
  left_join(data_optim, join_by(id_meta)) |> 
  select(id_meta, mean_d, mu_d)

tost <- with(data = data_model, expr = t_TOST(
  x = mu_d, y = mean_d, # Variables
  eqb = c(-0.17, 0.17), # Equivalence bounds
  paired = TRUE,        # Paired t-test
  var.equal = FALSE,    # Welch test
  hypothesis = "EQU",   # Equivalence test
  eqbound_type = "raw", # Raw bounds
  alpha = 0.05,         # Alpha level
  rm_correction = TRUE, # Repeated measure effect size correction
  smd_ci = "goulet"     # Goulet method to calculate CIs for SMD
))

write_rds(tost, here("data/src/model_h3.rds"))

# Plot TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
p <- plot_equ_tnull(tost, font = "Noto Sans Math") 
write_rds(p, file = here("data/src/plot_h3.rds"))

# Table TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
data_table <- chuck(tost, "TOST") |> 
  rownames_to_column("type") |> 
  mutate(type = case_when(
    type == "t-test" ~ "NHST",
    type == "TOST Lower" ~ "TOST $\\Delta < \\Delta_L$",
    type == "TOST Upper" ~ "TOST $\\Delta > \\Delta_L$"
  )) |>
  mutate(p.value = format_p(p.value, name = NULL)) |>
  mutate(p.value = str_remove(p.value, "^0")) |>
  mutate(t = format_value(t, digits = 2)) |>
  mutate(SE = format_value(SE, digits = 3))

table_h3 <- nice_table(
  x = data_table,
  caption = "Two One-Sided Tests Result for $\\mathcal{H}_3$", 
  digits = 2,
  col_names = c("Type", "$t$", "$SE$", "$df$", "$p$"),
  general_fn = "NHST: Null Hypothesis Significance Test, TOST: Two One-Sided Test"
  ) |>
  str_replace_all(string = _, pattern = "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")


cat(table_h3, file = here("tables/table_h3.tex"))
