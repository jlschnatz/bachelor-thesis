# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————
if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  # Fonts
  sysfonts, showtext, systemfonts,
  # File Management
  here, 
  # Data Manipulation & Analysis
  tidyverse, psych, corrr, vroom, glue, broom, insight,
  # Latex Math Expression
  latex2exp,
  # Visualization
  sjPlot, ggh4x, scales, paletteer, patchwork
)
# Source custom functions
source(here("R/00_functions.R"))
font_add_google("Noto Sans Math", "font")
showtext_auto()
showtext_opts(dpi = 500)
set.seed(42)

# Load data ————————————————————————————————————————————————————————————————————————————————————————————————————————————
data_meta <- read_csv(here("data/meta/processed/data_lindenhonekopp_proc.csv"))
data_optim <- read_csv(here("data/optim/processed/data_optim_merged.csv"))
id_mr <- unique(filter(data_meta, type_synthesis == "mr")$id_meta)

# Step 1: Check convergence ————————————————————————————————————————————————————————————————————————————————————————————

# For assessement of convergence of individual meta-analysis/multisite replication studies see `R/07_check_convergence.R`

# Step 2: Compare ML estimates for distributional parameters against SPEEC estimates ————————————————————————————————————

# Compute MLE Estimates of Distributional Parameters

## d ~ N(mu_d, sigma2_d)
ml_optim1 <- data_meta |>
  group_by(id_meta) |>
  summarise(norm = list(suppressWarnings(estimate_norm(d, method = "Nelder-Mead")))) |>
  mutate(norm = map(norm, tidy)) |>
  unnest(norm) |>
  mutate(parameter = case_match(parameter, "parameter1" ~ "ml_mu_d", "parameter2" ~ "ml_sigma2_d")) |>
  pivot_wider(names_from = parameter, values_from = value)

## n ~ NB(phi_n, mu_n)
ml_optim2 <- data_meta |>
  group_by(id_meta) |>
  summarise(nb = list(suppressWarnings(estimate_nb(n, method = "BFGS")))) |>
  mutate(nb = map(nb, tidy)) |>
  unnest(nb) |>
  mutate(parameter = case_match(parameter, "parameter1" ~ "ml_phi_n", "parameter2" ~ "ml_mu_n")) |>
  pivot_wider(names_from = parameter, values_from = value)

# k primary studies
data_k <- summarise(data_meta, k = n(), .by = id_meta)

# Join Data
data_ml_speec <- reduce(list(data_k, ml_optim1, ml_optim2, data_optim), inner_join, by = "id_meta")  |> 
  select(-c(runtime, bestval)) |>
  mutate(
    delta_mu_d = mu_d - ml_mu_d,
    delta_sigma2_d = sigma2_d - ml_sigma2_d,
    delta_phi_n = phi_n - ml_phi_n,
    delta_mu_n = mu_n - ml_mu_n
  ) |>
  # Calculate absolute differences
  mutate(across(starts_with("delta"), abs, .names = "abs_{.col}"))

# Correlation Data
cor_data <- data_ml_speec |>
  filter(id_meta %in% id_mr) |>
  select(starts_with("abs_delta"), w_pbs, k) |>
  rename_with(~ c(
    "\\Delta_{\\mu_d}", "\\Delta_{\\sigma^2_d}",
    "\\Delta_{\\phi_n}", "\\Delta_{\\mu_n}",
    "\\omega_{\\text{PBS}}",
    "k"
  )) |>
  rename_with(.cols = contains("Delta"), ~str_c("\\lvert", .x, "\\rvert")) |>
  rename_with(~glue("${.x}$")) |>
  relocate("$\\omega_{\\text{PBS}}$", .before = 1) |>
  corr.test( 
    minlength = 100,
    method = "pearson", 
    adjust = "BH", # Benjamini-Hochberg Correction
    alpha = 0.05
    ) 

# Confidence Bounds
ub_cor <- tanh(atanh(cor_data$r) + cor_data$se * qnorm(0.975))
lb_cor <- tanh(atanh(cor_data$r) - cor_data$se * qnorm(0.975))
p_star <- matrix(
  case_when(
    as.vector(cor_data$p) < 0.001 ~ "***",
    as.vector(cor_data$p) < 0.01 ~ "**",
    as.vector(cor_data$p) < 0.05 ~ "*",
    TRUE ~ ""
    ), nrow = 6, ncol = 6
  )

cor_data_format <- matrix(paste0(round(cor_data$r, 2), p_star,  " [", round(lb_cor, 2), ", ", round(ub_cor, 2), "]"), 6, 6) |> 
  as_cordf() |> 
  shave(upper = TRUE) |> 
  fashion()  

cor_data_format$term <- colnames(cor_data$r)
colnames(cor_data_format) <- c("Variable", colnames(cor_data$r))
cor_data_format <- cor_data_format[, -ncol(cor_data_format)]
  
general <-  paste(
  "Computed $p$-values are corrected for multiple comparison using the correction by Benjamini \\\\& Hochberg (1995).",
  "$\\\\lvert\\\\Delta\\\\rvert$ is the absolute difference for each distributional parameter between SPEEC and MLE.",
  sep = " "
  )
caption <- "Pairwise Pearson Correlations between the Absolute Difference of the Distributional Parameters from SPEEC and ML, Publication Bias Parameter and Meta-Analysis Size"

cor_table <- nice_table(
  x = cor_data_format,
  digits = 2,
  caption = caption,
  general_fn = general,
  symbol_fn = c("Significance *** $p$ < .001; ** $p$ < .01; * $p$ < .05")
) |>
  kableExtra::kable_styling(font_size = 10) |>
  kableExtra::column_spec(column = 1, width = "1.3cm") |>
  kableExtra::column_spec(column = 2:3, width = "3.5cm") |>
  kableExtra::column_spec(column = 4:5, width = "2.75cm") |>
  kableExtra::column_spec(column = 6, width = "2.2cm") 


cat(cor_table, file = here("tables/table_diagnostic_cormat.tex"))

# Desriptives Statistics: Quartile of the parameter estiamtes ————————————————————————————————————————————————————————————————————————————————————————————————————

caption_descr <- "Descriptive Statistics of the Discrepancy in the Distributional Parameter Estimates between SPEEC and MLE"
table_descr_discr <- data_ml_speec |> 
  select(starts_with("delta")) |>
  pivot_longer(cols = everything(), names_to = "parameter") |>
  group_by(parameter) |>
  summarise(fn = list(fivenum2(value))) |>
  unnest(fn) |>
  mutate(parameter = case_match(parameter,
  "delta_mu_d" ~ "$\\Delta_{\\mu_d}$",
  "delta_mu_n" ~ "$\\Delta_{\\mu_n}$",
  "delta_sigma2_d" ~ "$\\Delta_{\\sigma^2_d}$",
  "delta_phi_n" ~ "$\\Delta_{\\phi_n}$"
  )) |>
  nice_table(caption = caption_descr, col_names = c("Parameter", paste0("$Q_", 0:4, "$")), digits = 2)

#cat(table_descr_discr, file = here("tables/table_descr_discrepancy_ml_speec.tex"))

data_ml_speec |> 
  select(starts_with("delta")) |>
  pivot_longer(cols = everything(), names_to = "parameter") |>
  group_by(parameter) |>
  summarise(
    mean = mean(value), median = median(value), 
    iqr = diff(quantile(value, c(0.25, 0.75))),
    q10 = quantile(value, .1), q25 = quantile(value, .25), q75 = quantile(value, .75), q90 = quantile(value, .9)
    ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  write_rds(here("data/src/method_comparison_descr.rds"))

# Pairwise comparison plots: ML vs SPEEC

minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

plot_comparison <- function(data, x, y, color, scales) {
  enquo_x <- rlang::enquo(x)
  enquo_y <- rlang::enquo(y)
  enquo_color <- rlang::enquo(color)
  data |>
    filter(id_meta %in% id_mr) |>
    ggplot(aes(!!enquo_x, !!enquo_y)) +
    geom_point(
      aes(color = !!enquo_color),
      size = 2.5,
      alpha = .6
    ) +
    scale_y_continuous(
      name = scales$y$name,
      limits = scales$y$limits,
      breaks = scales$y$breaks,
      expand = expansion()
    ) +
    scale_x_continuous(
      name = scales$x$name,
      limits = scales$x$limits,
      breaks = scales$x$breaks,
      expand = expansion()
    ) +
    coord_equal() +
    scale_colour_paletteer_c(
    name = scales$color$name,
    palette = "pals::kovesi.linear_bmy_10_95_c78",
    limits = scales$color$limits,
    breaks = scales$color$breaks
    ) +
    theme_comparison()
}

# mu_n
p_mu_n <- data_ml_speec |>
  filter(id_meta %in% id_mr) |>
  ggplot(aes(ml_mu_n, mu_n)) +
  geom_abline(intercept = 0, slope = 1, color = "grey80", linewidth = .6) +
  geom_point(aes(color = abs_delta_mu_n), size = 2.5, alpha = .5) +
  scale_x_continuous(
    name = TeX("$\\widehat{\\mu}_{n_{ML}}$"),
    limits = c(0, 400),
    breaks = seq(0, 400, 100),
    expand = expansion()
  ) +
  scale_y_continuous(
    name = TeX("$\\widehat{\\mu}_{n_{SPEEC}}$"),
    limits = c(0, 400),
    expand = expansion(),
    breaks = seq(0, 400, 100),
  ) +
  coord_equal() +
  scale_colour_paletteer_c(
    name = TeX(r"($\Delta_{\mu_n}^{*}=|\widehat{\mu}_{n_{SPEEC}} - \widehat{\mu}_{n_{ML}}|$)"),
    palette = "pals::kovesi.linear_bmy_10_95_c78",
    limits = c(0, 180),
    breaks = seq(0, 180, 60)
  ) +
  theme_comparison()

# phi_n
p_phi_n <- data_ml_speec |>
  filter(id_meta %in% id_mr) |>
  ggplot(aes(ml_phi_n, phi_n)) +
  geom_abline(intercept = 0, slope = 1, color = "grey80", linewidth = .6) +
  geom_point(aes(color = abs_delta_phi_n), size = 2.5, alpha = .5) +
  scale_x_continuous(
    name = TeX("$\\widehat{\\phi}^2_{n_{ML}}$"),
    trans = log10_trans(),
    labels = label_log(),
    limits = c(10^-2, 10^3),
    breaks = c(10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3),
    minor_breaks = minor_breaks,
    expand = expansion()
  ) +
  scale_y_continuous(
    name = TeX("$\\widehat{\\phi}_{n_{SPEEC}}$"),
    trans = log10_trans(),
    labels = label_log(),
    limits = c(10^-2, 10^3),
    breaks = c(10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3),
    minor_breaks = minor_breaks,
    expand = expansion()
  ) +
  coord_equal() +
  scale_colour_paletteer_c(
    name = TeX(r"($\Delta_{\phi_n}^{*}=|\widehat{\phi}_{n_{SPEEC}} - \widehat{\phi}_{n_{ML}}|$)"),
    palette = "pals::kovesi.linear_bmy_10_95_c78",
    limits = 10**c(-1, 2),
    breaks = 10**seq(-1, 2), 
    trans = log10_trans(),
    labels = label_log(10)
  ) +
  theme_comparison() +
  theme(plot.margin = margin(r = 20))

# mu_d
p_mu_d <- data_ml_speec |>
  filter(id_meta %in% id_mr) |>
  ggplot(aes(ml_mu_d, mu_d, color = abs_delta_mu_d)) +
  geom_abline(intercept = 0, slope = 1, color = "grey80", linewidth = .5) +
  geom_point(
    size = 2.5,
    alpha = .6
    ) +
  scale_x_continuous(
    name = TeX("$\\widehat{\\mu}_{d_{ML}}$"),
    limits = c(-3, 3),
    breaks = seq(-3, 3, 1),
    expand = expansion()
  ) +
  scale_y_continuous(
    name = TeX("$\\widehat{\\mu}_{d_{SPEEC}}$"),
    limits = c(-3, 3),
    breaks = seq(-3, 3, 1),
    expand = expansion()
  ) +
  coord_equal() +
  scale_colour_paletteer_c(
    name = TeX(r"($\Delta_{\mu_d}^{*}=|\widehat{\mu}_{d_{SPEEC}} - \widehat{\mu}_{d_{ML}}|$)"),
    palette = "pals::kovesi.linear_bmy_10_95_c78",
    limits = c(0, .3),
    breaks = seq(0, 0.3, 0.1)
  ) +
  theme_comparison()

# sigma2_d
p_sigma2_d <- data_ml_speec |>
  filter(id_meta %in% id_mr) |>
  ggplot(aes(ml_sigma2_d, sigma2_d)) +
  geom_abline(intercept = 0, slope = 1, color = "grey80", linewidth = .5) +
  geom_point(
    aes(color = abs_delta_sigma2_d),
    size = 2.5,
    alpha = .6
  ) +
  scale_y_continuous(
    name = TeX("$\\widehat{\\sigma}^2_{d_{SPEEC}}$"),
    trans = log10_trans(),
    limits = c(0.001, 100),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = minor_breaks,
    labels = label_log(),
    expand = expansion()
  ) +
  scale_x_continuous(
    name = TeX("$\\widehat{\\sigma}^2_{d_{ML}}$"),
    trans = log10_trans(),
    limits = c(0.001, 100),
    labels = label_log(),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    minor_breaks = minor_breaks,
    expand = expansion()
  ) +
  coord_fixed() +
  scale_colour_paletteer_c(
    name = TeX(r"($\Delta_{\sigma^2_d}^{*}=|\widehat{\sigma}^2_{d_{SPEEC}} - \widehat{\sigma}^2_{d_{ML}}|$)"),
    palette = "pals::kovesi.linear_bmy_10_95_c78",
    limits = 10**c(-4, 1),
    breaks = 10**seq(-4, 1),
    labels = label_log(10),
    trans = log10_trans()
  ) +
  theme_comparison()

# Combine plots with patchwork
p_comb <- (p_mu_d | p_sigma2_d | p_mu_n | p_phi_n) +
  plot_layout(ncol = 4) +
  plot_annotation(tag_levels = c("A")) &
  theme(plot.tag = element_text(face = "bold", family = "font", margin = margin(l = 10)))

# Save
ggsave(
  plot = p_comb, 
  filename = here("figures/method_comparison.png"), 
  width = 11, height = 3.5, 
  bg = "white", dpi = 500
  )
