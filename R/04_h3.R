# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load(sysfonts, showtext, here, tidyverse, TOSTER, latex2exp,  systemfonts, kableExtra, ggrepel, sjPlot, rstatix)
source(here("R/functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Serif"
fontpath <- systemfonts::match_font(fontname)$path
font_add(family = fontname, fontpath)
font_add_google("Inter", "font")

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
  paired = TRUE, hypothesis = "EQU", eqbound_type = "raw",
  alpha = 0.05, rm_correction = TRUE, smd_ci = "goulet"
))

as.data.frame(data_model)[, "mean_d"] |> as.data.frame()

describe(tost)
as_htest(tost) |> describe_htest()
print(tost)

write_rds(tost, here("data/src/tost_model.rds"))

# Cohen´s d_rm
with(data_model, mean(mu_d - mean_d)/sd(mu_d - mean_d) * sqrt(2*(1-cor(mu_d, mean_d))))

# Plot TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
p <- plot_equ_tnull(tost, font = "Inter")  + theme(text = element_text(family = "font"))

write_rds(p, file = here("data/src/plot_h3.rds"))

# Table TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
data_table <- chuck(tost, "TOST") |> 
  rownames_to_column("type") |> 
  mutate(p.value = pformat(p.value)) 

table_h3 <- nice_table(
  x = data_table,
  caption = "TOST", 
  footnote = "test",
  digits = 2,
  col_names = c("Type", "$t$", "$SE$", "$df$", "$p$")
  )

cat(table_h3, file = here("tables/h3_table.tex"))

