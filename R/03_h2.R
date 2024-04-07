# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load(
  sysfonts, showtext, here, tidyverse, betareg, latex2exp, broom, 
  ggeffects, systemfonts, kableExtra, sjPlot, lmtest, insight
)
source(here("R/00_functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Sans Serif"
fontpath <- systemfonts::match_font(fontname)$path
font_add(family = fontname, fontpath)
showtext_opts(dpi = 500)
showtext_auto()

# Load data ————————————————————————————————————————————————————————————————————————————————————————————————————————————
data_meta <- read_csv(here("data/meta/processed/data_lindenhonekopp_proc.csv"))
data_optim <- read_csv(here("data/optim/processed/data_optim_merged.csv"))


# Beta-regression model specification: w_pbs ~ Delta**2 ————————————————————————————————————————————————————————————————————
data_model <- data_meta |> 
  group_by(id_meta) |> 
  summarise(mean_d = mean(d)) |> 
  left_join(data_optim) |> 
  select(id_meta, mean_d, mu_d, w_pbs) |> 
  mutate(Delta = mu_d - mean_d) 

# Fit model
mod_h2 <- betareg(
  formula = w_pbs ~ 1 + I(Delta**2),
  link = "logit",
  data = data_model
)

summary(mod_h2)

# Plot model predictions ———————————————————————————————————————————————————————————————————————————————————————————————————————
p <- ggeffect(mod_h2, "Delta [-2.8:2.9, by = 0.01]") |> 
  as_tibble() |> 
  ggplot(aes(x, y = predicted)) +
  geom_point(
    data = data_model, 
    mapping = aes(Delta, w_pbs), 
    alpha = 0.4,
    color = "#051088",
  ) +
  geom_line(linewidth = .7, color = "#051088") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, fill = "#051088")  +
  scale_y_continuous(
    name = TeX("$\\widehat{\\omega}_{PBS}$"),
    limits = c(0, 1),
    breaks = seq(0, 1, .2),
    expand = expansion(),
  ) +
  scale_x_continuous(
    name = latex2exp::TeX("Difference $\\Delta_{\\widehat{\\mu}_d, \\widehat{\\delta}}$"),
    limits = c(-3, 3),
    breaks = seq(-3, 3, .5),
    expand = expansion()
  ) +
  coord_cartesian(clip = "off") +
  theme_sjplot() +
  theme(
    text = element_text(family = fontname),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    axis.title.y = element_text(margin = margin(r = 0, unit = "mm")),
    axis.text.y = element_text(margin = margin(l = 0, unit = "mm")),
    axis.title.x = element_text(margin = margin(t = 8, unit = "mm"))
  )


ggsave(
  plot = p,
  filename = here("figures/h2_scatter.png"),
  width = 6, height = 4.5,
  bg = "white",
  dpi = 500
)

write_rds(p, file = here("data/src/plot_h2.rds"))

# Generate table with model results ————————————————————————————————————————————————————————————————————————————————————

z <- coef(mod_h2) / sqrt(diag(vcov(mod_h2)))
p_b2 <- pnorm(z[2], lower.tail = TRUE)

data_table <- tidy(mod_h2, conf.int = TRUE) |>
  # get OR for estimate and CIs for the mean parameter model
  mutate(across(c(estimate, conf.low, conf.high), exp)) |>
  # format confidence intervals
  mutate(ci = format_ci(conf.low, conf.high, ci_string = "", ci = NULL, digits = 2), .before = std.error) |>
  select(-starts_with("conf")) |>
  # format and correct p-values
  mutate(p.value = if_else(term == "I(Delta^2)", p_b2, p.value)) |>
  mutate(p.value = format_p(p.value, name = NULL, digits = "apa")) |>
  # remove leading zero
  mutate(p.value = str_remove(p.value, "^0")) |>
  mutate(across(c(estimate, std.error, statistic), ~format_value(.x, digits = 2))) |>
  mutate(estimate = if_else(component == "mean", paste0("$", estimate, "^a$"), paste0("$", estimate, "^b$"))) |>
  mutate(term = (case_match(term, "(phi)" ~ "Intercept", "(Intercept)" ~ "Intercept", "I(Delta^2)" ~ "Quadratic $\\Delta_{\\widehat{\\mu}_d, \\widehat{\\delta}}$"))) |> 
  select(-component) 

table_h2 <- nice_table(
  x = data_table, 
  caption = "Beta Regression Results for $\\mathcal{H}_2$", 
  digits = 2, 
  col_names = c("Term", "Estimate", "$CI$ (95\\%)","$SE$", "$z$", "$p$")
  ) |> 
  group_rows("Mean model component: $\\mu$", 1, 2, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") |>
  group_rows("Precision model component: $\\phi$", 3, 3, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") |>
  footnote(
    alphabet = c("$OR$", "Identity"),
    general = report_fit(mod_h2, "w_pbs")
  )

cat(table_h2, file = here("tables/h2_table.tex"))  

# Exploratory model ———————————————————————————————————————————————————————————————————————————————————————————————————————

# Fit
mod_h2_exploratory <- betareg(
  formula = w_pbs ~ I(Delta**2) | I(Delta**2),
  link = "logit",
  link.phi = "identity",
  data = data_model
)

summary(mod_h2_exploratory)

# Likelihood Ratio Test
model_comp <- lrtest(mod_h2, mod_h2_exploratory) |> 
  tidy() |> 
  mutate(across(where(is.numeric), ~round(.x, 3))) |> 
  mutate(across(everything(), as.character)) |> 
  mutate(across(everything(), ~replace_na(.x, "")))  |> 
  select(-df)

# Save Results
data_table_comparison <- nice_table(
  x = model_comp,
  caption = "Model comparison",
  digits = 3,
  col_names = c("Model", "$df$", "$\\log(\\mathcal{L})$", "$\\chi^2$", "$p$-value"),
  footnote = "Test"
)

cat(data_table_comparison, file = here("tables/h2_table_comparison.tex"))


