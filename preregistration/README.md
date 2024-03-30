## Preregistration of the Project

This folder contains the comprehensive preregistration of the project, which was submitted on the 8th March 2024. In addition to the preregistration itself the folder also contains the code for the sensitivity power analyses to determine the SESOI (smallest effect size of interest). This is structured into two R-scripts (in the `R/` folder):

- `sim_sesoi_power.R` to run the simulations, which are saved as .rds files (`h*_sim.rds`) in the `data/` folder.
- `plot_sesoi_power.R` to visualize the results of the simulations and to determine the SESOI

The figure of the power analysis is saved in the `img/` folder. The `src/` folder contains additional files for the rendering of the quarto file (bibliography, latex and csl files).
