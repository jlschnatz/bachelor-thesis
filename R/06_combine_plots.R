# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load(sysfonts, showtext, here, tidyverse, systemfonts, fs, patchwork)
source(here("R/functions.R"))
showtext_opts(dpi = 1000)
showtext_auto()

# Combine Plots into Panel ———————————————————————————————————————————————————————————————————————————————————————————       
file_paths <- dir_ls(here("data/src"), regexp = "plot")
plots <- purrr::reduce(map(file_paths, read_rds), `+`) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))

ggsave(
  filename = here("figures/combine_hypotheses.png"),
  plot = plots, width = 9, height = 6.5, dpi = 1000
  )
