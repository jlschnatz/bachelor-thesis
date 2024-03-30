# Load libraries ———————————————————————————————————————————————————————————————————————————————————————————————————————

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,  sysfonts, showtext, ggtext, ggsci, sjPlot, TOSTER, patchwork, geomtextpath, glue, here)

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Sans Serif"
fontpath <- systemfonts::match_font(fontname)$path
font_add(family = fontname, fontpath)
showtext_opts(dpi = 500)
showtext_auto()

scale_power <- scale_y_continuous(
  name = "Power",
  limits = c(0, 1),
  expand = c(0, 0),
  breaks = seq(0, 1, 0.2)
) 

theme_power <- theme_sjplot() +
  theme(
    axis.title.y = element_markdown(),
    legend.title = element_markdown(lineheight = 1.25),
    axis.title.x = element_markdown(margin = margin(t = 5)),
    axis.title.x.top = element_markdown(),
    text = element_text(family = fontname),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(5, 5, 5, 5, unit = "pt"),
  )

# Hypothesis I —————————————————————————————————————————————————————————————————————————————————————————————————————————

## Plot results
df_h1 <- read_rds(here("preregistration/data/h1_sim.rds"))

sesoi1 <- df_h1 |> 
  filter(phi == 10, power_mean > .8) |> 
  slice_min(power_mean) |> 
  mutate(b1 = exp(b1)) |> 
  pull(b1) |> 
  round(2)

df_h1 |>  
  mutate(b1 = exp(b1)) |>
  ggplot(aes(x = b1, y = power_mean, color = factor(phi))) +
  geom_textvline(
    aes(xintercept = sesoi1,
        label = glue::glue("SESOI = {sesoi1}")),
    color = "darkgrey",
    linewidth = 0.7,
    family = "os"
  ) +
  geom_line(linewidth = 0.7) +
  geom_ribbon(
    data = tibble(x = seq(1, 1.4, 0.1)),
    aes(ymin = 0.8, ymax = 1, x = x), fill = "grey", alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    mapping = aes(ymin = lb, ymax = ub, fill = factor(phi)),
    alpha = 0.3, 
    color = NA,
    show.legend = FALSE
    ) +
  scale_power +
  scale_x_continuous(
    name = "*OR* (b<sub>1</sub>)",
    limits = c(1, 1.4),
    breaks = seq(1, 1.4, 0.05),
    expand = c(0, 0),
    sec.axis = sec_axis(~ log(.), name = "log(*OR*)")
  ) +
  coord_cartesian(clip = "off") +
  scale_fill_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  scale_color_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  theme_power -> p1

print(p1)

# Hypothesis II —————————————————————————————————————————————————————————————————————————————————————————————————————————

## Plot results
df_h2 <- read_rds(here("preregistration/data/h2_sim.rds"))

sesoi2 <- df_h2 |> 
  filter(phi == 10, power_mean > .8) |>
  mutate(b2 = exp(b2)) |>
  slice_min(power_mean) |>
  pull(b2) |> 
  round(2)

df_h2 |> 
  mutate(b2 = exp(b2)) |> 
  ggplot(aes(x = b2, y = power_mean, color = factor(phi))) +
  geom_ribbon(
    data = tibble(x = seq(0.5, 1, 0.1)),
    aes(ymin = 0.8, ymax = 1, x = x),
    fill = "grey", color = NA,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    mapping = aes(ymin = lb, ymax = ub, fill = factor(phi)),
    alpha = 0.3, 
    color = NA,
    show.legend = FALSE
  ) +
  geom_textvline(
    aes(xintercept = sesoi2,
        label = glue::glue("SESOI = {sesoi2}")),
    color = "darkgrey",
    linewidth = 0.7,
    family = "os"
  ) +
  geom_line(linewidth = 0.7) +
  scale_power +
  scale_x_continuous(
    name = "*OR* (b<sub>2</sub>)",
    expand = expansion(),
    limits = c(0.5, 1),
    sec.axis = sec_axis(~ log(.), name = "log(*OR*)"),
  ) +
  scale_fill_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  scale_color_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  coord_cartesian(clip = "off") +
  theme_power -> p2

print(p2)

# Hypothesis III —————————————————————————————————————————————————————————————————————————————————————————————————————————

eqb <- read_rds(here("preregistration/data/h3_sim.rds"))
sesoi3 <- round(eqb, 2)
bounds <- seq(0.01, .25, 0.0005)

tibble(bounds, n = 150, alpha = 0.05) |> 
  mutate(power = map_dbl(
    .x = bounds, 
    .f = ~power_t_TOST(
      alpha = 0.05, 
      power = NULL, 
      n = 57, 
      delta = 0, 
      eqb = .x, 
      type = "paired", 
      sd = sqrt(0.3^2 + 0.3^2)
      )$power
    ))  |> 
  ggplot(aes(x = bounds, y = power)) +
  geom_ribbon(
    data = tibble(x = seq(0, 0.25, 0.01)),
    aes(ymin = 0.8, ymax = 1, x = x),
    fill = "grey", color = NA,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_textvline(
    aes(xintercept = sesoi3,
        label = glue::glue("SESOI = {sesoi3}")),
    color = "darkgrey",
    linewidth = 0.7,
    family = "os"
  ) +
  geom_line(linewidth = 0.7, col = "black") +
  scale_x_continuous(
    name = "Equivalence Bounds",
    limits = c(0, .25),
    breaks = seq(0, .25, 0.05),
    expand = expansion()
  ) +
  scale_power +
  coord_cartesian(clip = "off") +
  theme_power -> p3

print(p3)

# Hypothesis IV —————————————————————————————————————————————————————————————————————————————————————————————————————————

## Plot results
df_h4 <- read_rds(here("preregistration/data/h4_sim.rds"))


df_h4 |> 
  filter(phi == 10, power_mean > .8) |>
  mutate(or = exp(b1)) |>
  slice_min(power_mean) |> 
  pull(or) |>
  round(2) -> sesoi4

df_h4 |> 
  mutate(or = exp(b1)) |> 
  ggplot(aes(or, power_mean, color = factor(phi))) +
  geom_ribbon(
    data = tibble(x = seq(1, 1.5, 0.1)),
    aes(ymin = 0.8, ymax = 1, x = x),
    fill = "grey", color = NA,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    mapping = aes(ymin = lb, ymax = ub, fill = factor(phi)),
    alpha = 0.3, 
    color = NA,
    show.legend = FALSE
  ) +
  geom_textvline(
    aes(xintercept = sesoi4,
        label = glue::glue("SESOI = {sesoi4}")),
    color = "darkgrey",
    linewidth = 0.7,
    family = "os"
  ) +
  geom_line(linewidth = 0.7) +
  scale_power +
  scale_x_continuous(
    name = "*OR* (b<sub>1</sub>)",
    limits = c(1, 1.5),
    breaks = seq(1, 1.5, 0.1),
    expand = c(0, 0),
    sec.axis = sec_axis(~log(.), name = "log(*OR*)", breaks = seq(0, 0.5, 0.1)),
  ) +
  scale_fill_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  scale_color_sjplot(name = "Dispersion<br>parameter<br>&Phi;", palette = "quadro") +
  coord_cartesian(clip = "off") +
  theme_power  -> p4

print(p4)

# Save SESOIs
sesois <- tibble(
  hypothesis = c("H1", "H2", "H3", "H4"),
  sesoi = c(sesoi1, sesoi2, sesoi3, sesoi4),
  unit = c("OR", "OR", "raw", "OR")
)

write_csv(sesois, here("preregistration/data/sesois.csv"))

# Combine
(p1 | p2) / (p3 + p4) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "H<sub>", tag_levels = "1") &
  theme(plot.tag = element_markdown(family = fontname, face = "bold"))

ggsave("preregistration/img/power_sesoi_prereg.pdf", width = 12, height = 8, dpi = 300)

