# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(sysfonts, showtext, here, tidyverse, TOSTER, latex2exp,  systemfonts, kableExtra, ggrepel, sjPlot)
source(here("R/00_functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Serif"
fontpath <- systemfonts::match_font(fontname)$path
font_add(family = fontname, fontpath)
font_add_google("Lato", "font")
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

tost <- with(data_model, t_TOST(
  mu_d, mean_d, eqb = c(-0.17, 0.17), 
  paired = TRUE, var.equal = TRUE,
  hypothesis = "EQU", eqbound_type = "raw",
  alpha = 0.05, rm_correction = TRUE, smd_ci = "goulet"
))

write_rds(tost, here("data/src/model_tost_h3.rds"))

# Plot TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
p <- plot_equ_tnull(tost, font = "Open Sans") 
write_rds(p, file = here("data/src/plot_h3.rds"))

# Table TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
data_table <- chuck(tost, "TOST") |> 
  rownames_to_column("type") |> 
  as_tibble() |>
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
  ) 

cat(table_h3, file = here("tables/h3_table.tex"))

