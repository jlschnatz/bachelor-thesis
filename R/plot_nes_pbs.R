if (!"pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)
p_load(latex2exp, sysfonts, showtext, dplyr, purrr, tidyr, ggplot2, forcats, ggh4x, systemfonts)

# Add fonts ————————————————————————————————————————————————————————————————————————————————————————————————————————————
fontname <- "CMU Bright"
fontpath <- match_font(fontname)$path
font_add(family = fontname, fontpath)
showtext_opts(dpi = 500)
showtext_auto()

p_from_nes <- function(d, n1, n2, method = c("less", "two-sided", "greater")) {
  t <- d / sqrt((1 / n1) + (1 / n2))
  df <- n1 + n2 - 2
  switch(method,
         "less" = pt(t, df, lower.tail = TRUE),
         "two-sided" = 2 * pt(-abs(t), df),
         "greater" = pt(t, df, lower.tail = FALSE)
         )
}

get_2n <- function(n) {
  stopifnot(is.numeric(n))
  cbind(ifelse(n %% 2 == 0, n/2, ceiling(n/2)), ifelse(n %% 2 == 0, n/2, floor(n/2)))
}

simulate_pbs <- function(k, n_phi, n_mu, d_mu, d_sigma, w_pbs, method = c("less", "two-sided", "greater")) {
  n <- rnbinom(k, size = n_phi, mu = n_mu)
  se <- d_sigma/sqrt(n)
  d <- rnorm(k, mean = d_mu, sd = se/mean(se)*d_sigma)
  p <- p_from_nes(d, get_2n(n)[, 1], get_2n(n)[, 2], method = method)
  w <- ifelse(p < .05, 1, w_pbs)
  selected <- as.logical(rbinom(k, 1, w))
  prob_surv <- mean(selected)
  k_adj <- ceiling(k / prob_surv)
  n_adj <- rnbinom(k_adj, size = n_phi, mu = n_mu)
  se_adj <- d_sigma/sqrt(n_adj)
  d_adj <- rnorm(k_adj, mean = d_mu, sd = se_adj/mean(se_adj)*d_sigma)
  p_adj <- p_from_nes(d_adj, get_2n(n_adj)[, 1], get_2n(n_adj)[, 2], method = method)
  w_adj <- ifelse(p_adj < .05, 1, w_pbs)
  selected_adj <- as.logical(rbinom(k_adj, 1, w_adj))
  out <- data.frame(n = n_adj, d = d_adj)[selected_adj, ]
  return(out)
}

grid <- expand_grid(
  k = 1e4, n_phi = 10, n_mu = 100, d_mu = c(0, .4), d_sigma = 0.4,  
  w_pbs = c(1, 0), 
  method = c("greater", "two-sided")
) 

set.seed(42)
data_plot <- grid |> 
  mutate(data = pmap(list(k, n_phi, n_mu, d_mu, d_sigma, w_pbs, method), simulate_pbs)) |> 
  unnest(data)  |> 
  mutate(d_sign = d, d_unsign = abs(d)) |>
  select(-d) |> 
  pivot_longer(cols = c(d_sign, d_unsign), names_to = "type_sign", values_to = "d") |> 
  mutate(
    type_sign = case_when(
      type_sign == "d_sign" ~ "'d'",
      type_sign == "d_unsign" ~ "'|d|'"
    ),
    delta = as_factor(case_when(
      d_mu == 0 ~ "delta*' = 0'",
      d_mu == 0.4 ~ "delta*' = 0.4'"
    )),
    w_pbs = as_factor(case_when(
      w_pbs == 1 ~ "omega[PBS]*' = 1'",
      w_pbs == 0 ~ "omega[PBS]*' = 0'"
    )),
    method = as_factor(case_when(
      method == "greater" ~ "H[1]*': ' *theta * {phantom() > phantom()} * 0",
      method == "two-sided" ~ "H[1]*': '*theta*' ' * {phantom() != phantom()} * ' 0' "
    ))
  )

data_plot |> 
  select(w_pbs, type_sign, method, delta) |> 
  distinct() |> 
  arrange(delta,method, w_pbs, desc(type_sign)) |> 
  mutate(id = LETTERS[seq_len(n())]) -> label_data

ggplot(data_plot, aes(n, d)) +
  facet_nested(
    rows = vars(delta, method),
    cols = vars(w_pbs, type_sign),
    labeller = label_parsed
    ) +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = 0.3) +
  geom_point(alpha = 0.1, size = 0.2, shape = 21) +
  geom_smooth(linewidth = 0.9, method = "lm", se = FALSE, color = "#26b999") +
  geom_text(
    data = label_data,
    mapping = aes(x = 290, y = 1.5, label = id),
    hjust   = 1,
    #vjust   = -1,
    fontface = "bold",
    family = "os"
  ) +
  scale_y_continuous(
    name = "Effect size",
    limits = c(-2.5, 2.5),
    breaks = seq(-2, 2, 1),
    expand = expansion()
  ) +
  scale_x_continuous(
    name = "Sample size",
    limits = c(0, 300),
    breaks = seq(0, 300, 100),
    expand = expansion()
  ) +
  sjPlot::theme_sjplot() +
  theme(
    text = element_text(family = fontname),
    panel.spacing = unit(0.8, "lines"),
    panel.grid.minor = element_blank()
  ) -> p

ggsave(
  plot = p, filename = "figures/nes_correlation_grid.png",
  width = 9, height = 8, dpi = 400, bg = "white")

