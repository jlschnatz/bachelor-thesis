# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  tidyverse, betareg, here, sysfonts, showtext,
  marginaleffects, ggdist, latex2exp, systemfonts, kableExtra,
  sjPlot, insight
)
source(here("R/00_functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Sans Serif"
fontpath <- match_font(fontname)$path
font_add(family = fontname, fontpath)
font_add_google("Inter", "font")
showtext_opts(dpi = 500)
showtext_auto()

# Load data ————————————————————————————————————————————————————————————————————————————————————————————————————————————
data_meta <- read_csv(here("data/meta/processed/data_lindenhonekopp_proc.csv"))
data_optim <- read_csv(here("data/optim/processed/data_optim_merged.csv"))

# Beta-regression model specification: w_pbs ~ type_synthesis ————————————————————————————————————————————————————————————————————
data_model <- data_meta |> 
  distinct(id_meta, type_synthesis) |> 
  left_join(data_optim, by = join_by(id_meta)) |>
  select(id_meta, type_synthesis, w_pbs) |> 
  mutate(type_synthesis = factor(type_synthesis, levels = c("ma", "mr"), labels = c("Meta-Analyses", "Multisite Replications"))) 

contrasts(data_model$type_synthesis)

# Fit model
mod_h4 <- betareg(
  formula = w_pbs ~ type_synthesis ,
  link = "logit",
  link.phi = "identity",
  data = data_model
  ) 

summary(mod_h4)
write_rds(mod_h4, here("data/src/model_betareg_h4.rds"))

# Plot model predictions ————————————————————————————————————————————————————————————————————————————————————————————————————
set.seed(42)
p <- avg_predictions(mod_h4, by = "type_synthesis") |> 
  as_tibble() |> 
  ggplot(aes(type_synthesis, estimate, color = type_synthesis)) +
  geom_jitter(
    data = data_model, 
    mapping = aes(y = w_pbs),
    #color = "#051088",
    width = 0.025,
    alpha = 0.2,
    size = 1
    ) +
  stat_slab(
    data = data_model, 
    mapping = aes(y = w_pbs, fill = type_synthesis),
    alpha = .6,
    normalize = "groups",
    #fill = "#051088",
    adjust = .8,
    trim = TRUE,
    color = NA,
    position = position_nudge(x = .1),
    scale = 0.3
    ) +
  geom_linerange(
    mapping = aes(ymin = conf.low, ymax = conf.high, color = type_synthesis),
    linewidth = .7,
    #color = "#051088",
    position = position_nudge(x = -0.15)
    ) +
  geom_point(
    size = 2,
    #color = "#051088",
    mapping = aes(color = type_synthesis),
    position = position_nudge(x = -0.15)
    ) +
  scale_y_continuous(
    name = latex2exp::TeX("$\\widehat{\\omega}_{PBS}$"),
    limits = c(0, 1),
    expand = expansion()
  ) +
  xlab(NULL) +
  coord_cartesian(clip = "off") +
  guides(color = "none", fill = "none") +
  scale_fill_manual(
    values = c("#051088", "#f7ce4c"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("#051088", "#f7ce4c"),
    guide = "none"
  ) +
  theme_sjplot() +
  theme(
    text = element_text(family = "font"),
    axis.title = element_text(size = 10),
    panel.background = element_rect(color = "grey", linewidth = 0.5)
    ) 

write_rds(p, file = here("data/src/plot_h4.rds"))

# Generate table with model results ————————————————————————————————————————————————————————————————————————————————————————————
z <- coef(mod_h4) / sqrt(diag(vcov(mod_h4)))
p_b1 <- pnorm(z[2], lower.tail = FALSE)

data_table <- tidy(mod_h4, conf.int = TRUE) |>
  # get OR for estimate and CIs for the mean parameter model
  mutate(across(c(estimate, conf.low, conf.high), exp)) |>
  # format confidence intervals
  mutate(ci = format_ci(conf.low, conf.high, ci_string = "", ci = NULL, digits = 2), .before = std.error) |>
  select(-starts_with("conf")) |>
  # format and correct p-values
  mutate(p.value = if_else(term == "type_synthesisMultisite Replications", p_b1, p.value)) |> 
  mutate(p.value = format_p(p.value, name = NULL, digits = "apa")) |>
  # remove leading zero
  mutate(p.value = str_remove(p.value, "^0")) |>
  mutate(across(c(estimate, std.error, statistic), ~format_value(.x, digits = 2))) |>
  mutate(estimate = if_else(component == "mean", paste0("$", estimate, "^a$"), paste0("$", estimate, "^b$"))) |>
  mutate(term = case_match(term, "(Intercept)" ~ "Intercept", "type_synthesisMultisite Replications" ~ "Research Synthesis Type (MR)", "(phi)" ~ "Intercept")) |>  
  select(-component) 

table_h4 <- nice_table(
  x = data_table, 
  caption = "Beta Regression Results for $\\mathcal{H}_4$", 
  col_names = c("Term", "Estimate", "$CI$ (95\\%)","$SE$", "$z$", "$p$"),
  digits = 2,
  general_fn = glue::glue("MR: Multisite Replication; {report_fit(mod_h4, 'w_pbs')}"),
  alphabet_fn = c("$OR$", "Identity coefficient")
) |> 
  group_rows("Mean model component: $\\mu$", 1, 2, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") |>
  group_rows("Precision model component: $\\phi$", 3, 3, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") 

cat(table_h4, file = here("tables/h4_table.tex"))
