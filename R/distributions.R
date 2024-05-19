#!/usr/bin/env Rscript

# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load(here, tidyverse, distributional, ggdist, patchwork, latex2exp, sysfonts, showtext, sjPlot)
font_add_google("Noto Sans Math", "font")
showtext_auto()
showtext_opts(dpi = 500)
set.seed(42)
source(here("R/functions.R"))

nb_grid <- expand.grid(phi = c(5, 25, 100), mu = c(50, 100, 250))
nb_dist <- with(with(nb_grid, reparametrize_nb(phi, mu)), dist_negative_binomial(size, p))
df_nb <- tibble(nb_dist, nb_grid) |> mutate(across(phi:mu, as.factor)) 
col <- pals::kovesi.linear_bmy_10_95_c78(n = length(unique(df_nb$mu)))
p1 <- ggplot(df_nb, aes(xdist = nb_dist, y = mu, fill = phi)) +
    stat_histinterval(
        geom = "slab", density = "histogram", 
        alpha = 0.75, p_limits = c(0, 0.9),
        scale = 0.8,
        normalize = "xy"
        ) +
    scale_fill_manual(values = col, name = TeX("$\\phi_n$")) +
    scale_color_manual(values = col, name = TeX("$\\phi_n$")) +
    ylab(TeX("$\\mu_n$")) +
    xlab(NULL) +
    scale_y_discrete(expand = expansion(mult = c(0.1, -1))) +
    scale_x_continuous(
        limits = c(0, 600),
        expand = expansion(mult = c(0.05, 0))
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    sjPlot::theme_sjplot() +
    theme(
        panel.background = element_rect(color = "grey"),
        legend.position = "bottom",
        text = element_text(family = "font"),
        axis.title.x = element_blank(),
        legend.margin = margin(),
        legend.box.margin = margin()
    )
  

norm_grid <- expand.grid(mu = c(0, 0.2, 0.5), sigma2 = c(0.05, .25, .5))
norm_dist <- with(norm_grid, dist_normal(mu, sqrt(sigma2)))
df_norm <- tibble(norm_grid, norm_dist) |> mutate(across(sigma2:mu, as.factor)) 
col <- pals::kovesi.linear_bmy_10_95_c78(n = length(unique(df_norm$sigma2)))
p2 <- ggplot(df_norm, aes(xdist = norm_dist, y = mu, fill = sigma2)) +
    stat_histinterval(
        geom = "slab", density = "histogram", 
        alpha = 0.75, p_limits = c(0.01, 0.99),
        scale = 0.8,
        normalize = "xy"
        ) +
    scale_fill_manual(values = col, name = TeX("$\\sigma^2_d$")) +
    ylab(TeX("$\\mu_d$")) +
    xlab(NULL) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
    scale_x_continuous(limits = c(-2, 2), expand = expansion()) +
    sjPlot::theme_sjplot() +
    theme(
        panel.background = element_rect(color = "grey"),
        legend.position = "bottom",
        text = element_text(family = "font"),
        axis.title.x = element_blank(),
        legend.margin = margin(),
        legend.box.margin = margin()
    )


p3 <- tibble(
    w_pbs = seq(0.008, 1, 0.0001),
    inv = 1 / w_pbs
    ) |>
    ggplot(aes(x = w_pbs, y = inv)) +
    geom_line(
        col = "#000C7D",
        linewidth = 0.8
        ) +
    scale_y_continuous(
        name = expression(1 / omega[PBS]),
        expand = expansion(),
        limits = c(0, 2^7),
        breaks = c(2, 8, 16, 32, 64, 128),
    ) +
    scale_x_continuous(
        name = TeX("$\\omega_{PBS}$"),
        limits = c(0, 1),
        breaks = seq(0, 1, .2),
        expand = expansion(mult = c(0.05, 0))
    ) +
    coord_cartesian(clip = "off") +
    theme_sjplot() +
    theme(
        plot.margin = margin(r = 15, t = 15),
        text = element_text(family = "font")
    )

font_add_google("Inter", "font2")
(free(p1) + free(p2)) + free(p3) +
    plot_annotation(tag_levels = "A") &
    theme(
        plot.tag = element_text(face = "bold", family = "font2"),
        plot.margin = margin(r = 5)
        )

ggsave(here("figures/distributions.png"), height = 3.5, width = 9, dpi = 500, bg = "white")