#!/usr/bin/env Rscript


# Install, load packages & functions ———————————————————————————————————————————————————————————————————————————————————       
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(sysfonts, showtext, here, tidyverse, fs, patchwork, grid, gridtext)
source(here("R/functions.R"))
showtext_opts(dpi = 1000)
font_add_google("Inter", "font")
showtext_auto()

# Combine Plots into Panel ———————————————————————————————————————————————————————————————————————————————————————————       
file_paths <- dir_ls(here("data/src"), regexp = "plot")
plots <- (reduce(map(file_paths, read_rds), `+`) + plot_annotation(tag_levels = "A")) &
    theme(plot.tag = element_text(face = "bold", family = "font"))

ggsave(
  filename = here("figures/hypotheses_multipanel.png"),
  plot = plots, width = 9, height = 6.5, dpi = 1000
  )
