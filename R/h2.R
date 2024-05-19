#!/usr/bin/env Rscript


# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  tidyverse,                                  # data manipulation
  here,                                       # file management
  sysfonts, showtext,                         # fonts
  betareg, insight, broom, ggeffects, lmtest, # modeling
  latex2exp,                                  # latex equation
  kableExtra                                  # tables

)
# Load functions
source(here("R/functions.R"))

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
font_add_google("Noto Sans Math", "font")
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
  data = data_model,
  control = betareg.control(method = "L-BFGS", trace = F)
)
summary(mod_h2)
write_rds(mod_h2, here("data/src/model_h2.rds"))

# Plot model predictions ———————————————————————————————————————————————————————————————————————————————————————————————————————
p <- ggeffect(mod_h2, "Delta [-.7:.7, by = 0.01]") |> 
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
    name = latex2exp::TeX("Difference $\\Delta_{\\widehat{\\mu}_d}$"),
    limits = c(-.8, .8),
    breaks = seq(-.8, .8, .2),
    expand = expansion()
  ) +
  coord_cartesian(clip = "off") +
  sjPlot::theme_sjplot() +
  theme(
    text = element_text(family = "font"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    axis.title.y = element_text(margin = margin(r = 0, unit = "mm")),
    axis.text.y = element_text(margin = margin(l = 0, unit = "mm")),
    axis.title.x = element_text(margin = margin(t = 8, unit = "mm"))
  )

write_rds(p, file = here("data/src/plot_h2.rds"))

# Generate table with model results ————————————————————————————————————————————————————————————————————————————————————
z <- coef(mod_h2) / sqrt(diag(vcov(mod_h2)))
p_b2 <- pnorm(z[2], lower.tail = TRUE)

data_table <- tidy(mod_h2, conf.int = TRUE) |>
  # one-sided confidence interval for Delta^2 -> upper limits is 0
  mutate(conf.low = if_else(term == "I(Delta^2)", -Inf, conf.low)) |>
  # recalculate high CI for Delta^2
  mutate(conf.high = if_else(term == "I(Delta^2)", estimate + qnorm(0.95) * std.error, conf.high)) |>
  # transform CIs and estimate for log-OR to ORs
  mutate(across(c(estimate, conf.low, conf.high), exp)) |>
  mutate(ci = format_ci(conf.low, conf.high, ci_string = "", ci = NULL, digits = 2), .before = std.error) |>
  select(-starts_with("conf")) |>
  mutate(p.value = if_else(term == "I(Delta^2)", p_b2, p.value)) |>
  mutate(p.value = str_remove(format_p(p.value, name = NULL, digits = "apa"), "^0")) |>
  mutate(across(c(estimate, std.error, statistic), ~format_value(.x, digits = 2))) |>
  mutate(estimate = if_else(component == "mean", paste0("$", estimate, "^a$"), paste0("$", estimate, "^b$"))) |>
  mutate(ci = if_else(term == "I(Delta^2)", glue::glue("${ci}^c$"), ci)) |>
  mutate(term = case_match(term, 
    "(phi)" ~ "Intercept", 
    "(Intercept)" ~ "Intercept",
    "I(Delta^2)" ~ "$\\Delta_{\\mu_d}^2$")
    ) |> 
  select(-component) 


table_h2 <- nice_table(
  x = data_table,
  caption = "Beta Regression Results for $\\hypothesis{2}{}$",
  digits = 2,
  col_names = c("Term", "Estimate", "$CI$ (95\\%)","$SE$", "$z$", "$p$"),
  alphabet_fn = c("$OR$", "Raw values", "One-sided confidence interval in direction of the hypothesis"),
  general_fn = paste0("$CI$: Confidence interval, $SE$: standard error, ", report_fit(mod_h2, "w_pbs"))
) |>
  group_rows("Mean model component: $\\mu$", 1, 2, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") |>
  group_rows("Precision model component: $\\phi$", 3, 3, escape = FALSE, extra_latex_after = "\\\\[-1.5ex]") |>
  str_replace_all(string = _, pattern = "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")

cat(table_h2, file = here("tables/table_h2.tex"))  

# Exploratory model ———————————————————————————————————————————————————————————————————————————————————————————————————————
# Fit
mod_h2_exploratory <- betareg(
  formula = w_pbs ~ I(Delta**2) | I(Delta**2),
  link = "logit",
  link.phi = "identity",
  data = data_model,
  control = betareg.control(method = "BFGS", trace = TRUE)
)

# Likelihood Ratio Test
model_comp <- lrtest(mod_h2, mod_h2_exploratory) 
print(model_comp) # n.s.
