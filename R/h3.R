#!/usr/bin/env Rscript

# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(
  tidyverse,          # data manipulation
  sysfonts, showtext, # fonts
  here,               # file management
  latex2exp,          # latex formula
  kableExtra,         # tables
  TOSTER, insight,     # modeling
  ggpubr, rstatix, boot,
  latex2exp, sjPlot,
  sjPlot, ggdist, ggtext, ggnewscale, distributional
  )
source(here("R/functions.R"))

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

# Testing Assumptions  ————————————————————————————————————————————————————————————————————————————————————————————————

# Shapiro Test:
shapiro_data <- data_model |> 
  pivot_longer(-id_meta, names_to = "type") |>
  group_by(type) |> 
  shapiro_test(value) |>
  mutate(type = if_else(
    type == "mean_d", 
    "Average effect size $\\widehat{\\delta}$",
     "Mean parameter $\\widehat{\\mu}_d$ from SPEEC")
     ) |>
  mutate(p = format_p(p, name = "")) |>
  select(-variable)


table_shapiro <- nice_table(
  x = shapiro_data,
  caption = "Shapiro-Wilk Test Testing Normality for $\\hypothesis{3}{}$",
  col_names = c("Parameter", "$W$", "$p$"),
  general_fn = "$W$: Shapiro-Wilk test statistic"
) |>
  column_spec(column = 1, width = "7cm") |>
  column_spec(column = 2:3, width = "4.5cm")

cat(table_shapiro, file = here("tables/table_shapiro_h3.tex"))

p_shapiro <- data_model  |>
  pivot_longer(-id_meta, names_to = "type") |>
  mutate(type = if_else(
    type == "mean_d", 
    "'Average Effect Size '*widehat(delta)", 
    as.character(TeX("Mean Parameter $\\widehat{\\mu}_d$ from SPEEC"))
    )) |>
  ggqqplot(data = _, x = "value", color = "type") +
  facet_wrap(~type, labeller = label_parsed) +
  scale_fill_manual(values = c("#051088", "#f7ce4c")) +
  scale_color_manual(values = c("#051088", "#f7ce4c")) +
  guides(fill = "none", color = "none") +
  scale_x_continuous(expand = c(0, 0), limits = c(-3, 3)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-3, 3)) +
  scale_alpha_continuous(range = c(0.1, .1)) +
  coord_equal() +
  theme_sjplot() +
  theme(
    text = element_text(family = "font"),
    panel.spacing = unit(0.5, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 11)
    )

ggsave(
  filename = here("figures/h3_qqplot.png"), 
  plot = p_shapiro, width = 8, height = 4, 
  dpi = 500, bg = "white"
  )


# Violation of Normality Assumption for TOST procedure 

levene_data <- data_model |>
  pivot_longer(-id_meta, names_to = "type") |>
  mutate(type = factor(type)) |>
  levene_test(value ~ type)

write_rds(levene_data, here("data/src/asmpt_levene_h3.rds"))



eqb <- c(-0.17, 0.17)

# Wilxocon signed rank test sensitivty analyses
tost_wilcox <- with(data_model, wilcox_TOST(
  x = mu_d, y = mean_d,
  eqb = eqb,            # Equivalence bounds
  paired = TRUE,        # Paired test
  hypothesis = "EQU",   # Equivalence test
  alpha = 0.05,          # Alpha level
))

# -> same result as t-test

tost_t <- with(data = data_model, expr = t_TOST(
  x = mu_d, y = mean_d, # Variables
  eqb = eqb, # Equivalence bounds
  paired = TRUE,        # Paired t-test
  hypothesis = "EQU",   # Equivalence test
  eqbound_type = "raw", # Raw bounds
  alpha = 0.05,         # Alpha level
  rm_correction = TRUE, # Repeated measure effect size correction
  smd_ci = "goulet"     # Goulet method to calculate CIs for SMD
))

write_rds(tost_t, here("data/src/model_h3.rds"))

data_plot <- as_tibble(tost_t$effsize[1, ])
df <- tost_t$TOST$df[1]
buffer <- 0.075
data_text <- tibble::tibble(
    type = c("lower", "upper"),
    bounds = c(tost_t$eqb$low_eq[1], tost_t$eqb$high_eq[1]),
    x = if_else(type == "lower", bounds + buffer, bounds - buffer),
    label = dplyr::if_else(type == "lower", paste0("&Delta;<sub>L</sub> = ", round(bounds, 2)), paste0("&Delta;<sub>U</sub> = ", round(bounds, 2)))
  ) 
dist <- dist_student_t(df = df, mu = data_plot$estimate, sigma = data_plot$SE, ncp = 0)
dist_eq_null <-  dist_student_t(df = df, mu = eqb, sigma = data_plot$SE) 

p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey80", linewidth = 0.4) +
  geom_richtext(
    data = data_text,
    mapping = aes(y = 1, x = bounds, label = label), 
    color = NA,
    size = 3,
    text.colour = "black",
    fill = NA,
    family = "font",
    inherit.aes = FALSE,
    hjust = c(0, 1)
    ) +
  stat_dist_slab(
    mapping = aes(dist = dist, y = 0, fill = after_stat(cut_cdf_qi(p = cdf, .width = c(0.68, 0.9, 0.95, 0.99)))),
    alpha = 0.9,
    slab_size = 0.5
  ) +
  stat_dist_pointinterval(aes(dist = dist, y = 0), .width = 0.95, linewidth = 0.4, position = position_nudge(y = -0.05)) +
  scale_fill_manual(
     name = "CI",
      values = c("#000C7D", "#A7018B", "#FF7051", "#FFF123"),
      na.translate = FALSE
     ) +
   new_scale_fill() +
   stat_slab(
      ggplot2::aes(fill = ggplot2::after_stat(level), dist = dist_eq_null, y = 0),
      slab_size = 0.5,
      .width = c(.9, 1),
      show.legend = FALSE
    ) +
   scale_fill_manual(values = (c("black", "grey90")))  +
   geom_vline(data = data_text, aes(xintercept = bounds), linetype = "longdash", linewidth = 0.35) +
   scale_y_continuous(
      name = NULL,
      breaks = NULL,
      limits = c(-0.1, 1.25),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      limits = c(-.3, .3),
      breaks = seq(-.3, .3, .1),
      labels = scales::label_number(),
      expand = c(0, 0),
      name = "Mean Difference"
    ) +
    coord_cartesian(clip = "off") +
    theme_sjplot() +
    theme(
      plot.margin  = ggplot2::margin(t = 0, l = 0, 1, 1, unit = "lines"),
      panel.background = element_rect(color = "grey", linewidth = 0.5),
      axis.title = element_text(size = 10),
      text = element_text(family = "font"),
      panel.grid.major = element_line(linewidth = 0.2),
      panel.ontop = FALSE,
      legend.title = element_text(size = 10, vjust = 0.5),
      legend.text = element_text(size = 8),
      legend.key.spacing = unit(2, "pt"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.position = "top",
      legend.title.position = "left",
      legend.margin = margin(b = -10),
      legend.text.position = "left"
    ) 

write_rds(p, file = here("data/src/plot_h3.rds")) # save


# calculate proportional distance according to Gutierrez & Cribbie (2023)
lambda <- mean(with(data_model, mu_d - mean_d)) # observed mean difference
ei_sign <- eqb[which(sign(eqb) == sign(lambda))]    # bound of the equivalence interval; same sign as the observed effect
lambda_prime <- 0                               # effect under h1
pd <- (lambda - lambda_prime)/abs((ei_sign - lambda_prime))
boots <- boot(
  data = with(data_model, mu_d - mean_d),
  statistic = function(data, i) mean(data[i]),
  R = 1e3
)
bca_ci <- confint(boots, type = "bca")/abs((ei_sign - lambda_prime))
data_pd <- tibble(pd, lb = bca_ci[1], ub = bca_ci[2])

write_rds(data_pd, here("data/src/es_pd_h3.rds"))

# Table TOSTER Results ———————————————————————————————————————————————————————————————————————————————————————————————————————
data_table <- chuck(tost_t, "TOST") |> 
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

n <- nrow(data_model)
E_Tplus <- n * (n + 1) / 4
Sigma2_Tplus <- n * (n + 1) * (2 * n + 1) / 24

data_table_wilcox <- chuck(tost_wilcox, "TOST") |>
  rownames_to_column("type") |> 
  mutate(hypothesis = case_when(
    type == "NHST" ~ "$\\Delta = 0$",
    type == "TOST Lower" ~ "$\\Delta < \\Delta_L$",
    type == "TOST Upper" ~ "$\\Delta > \\Delta_L$"
    )) |>  
  mutate(E_Tplus, Sigma_Tplus = sqrt(Sigma2_Tplus)) |>
  mutate(z = case_when(
    type == "NHST" ~ (statistic - E_Tplus - .5)/Sigma_Tplus,
    type == "TOST Lower" ~ (statistic - E_Tplus + 0.5)/Sigma_Tplus,
    type == "TOST Upper" ~ (statistic - E_Tplus - 0.5)/Sigma_Tplus
  )) |>
  mutate(type = case_when(
    type == "NHST" ~ "NHST",
    type == "TOST Lower" ~ "TOST",
    type == "TOST Upper" ~ "TOST"
    )) |>
  mutate(p.value = format_p(p.value, name = NULL, digits = 3)) |>
  mutate(p.value = str_remove(p.value, "^0")) |>
  relocate(c(E_Tplus, Sigma_Tplus, z), .after = statistic) |>
  relocate(hypothesis, .after = type)


table_h3_wilcox <- nice_table(
  x = data_table_wilcox,
  col_names = c("Type", "Hypothesis", "$T^+$", "$\\mu_{T^+}$", "$\\sigma_{T^+}$", "$z$", "$p$"),
  caption = "Sensitivity Analyses for Hypotheses III: Wilxocon Signed-Rank Test",
  general_fn = paste("Continuity correction applied. Approximate Gaussian null distribution used. $T^+$: positive rank sum (test statistic), ", 
                      "$\\mu_{T^+}$: mean of positive rank sum under $\\mathcal{H}_0$, ",
                      "$\\sigma_{T^+}$: variance of positive rank sum under $\\mathcal{H}_0$") 
) |>
  str_replace_all(string = _, pattern = "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")


cat(table_h3_wilcox, file = here("tables/table_wilcox_h3.tex"))

table_h3 <- nice_table(
  x = data_table,
  caption = "Two One-Sided Tests Result using Welch´s Tests regarding $\\mathcal{H}_3$", 
  digits = 2,
  col_names = c("Type", "$t$", "$SE$", "$df$", "$p$"),
  general_fn = "NHST: Null Hypothesis Significance Test, TOST: Two One-Sided Test"
  ) |>
  str_replace_all(string = _, pattern = "\\\\begin\\{tablenotes\\}", "\\\\begin\\{tablenotes\\}[flushleft]")

cat(table_h3, file = here("tables/table_h3.tex"))
