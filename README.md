# My Project


# Bachelor Thesis

## Project Structure

`R/`: contains all R-scripts for data analysis and visualization

    #> R/
    #> ├── 00_functions.R
    #> ├── 01_process_data.R
    #> ├── 02_h1.R
    #> ├── 03_h2.R
    #> ├── 04_h3.R
    #> ├── 05_h4.R
    #> ├── 06_combine_plots.R
    #> ├── 07_check_validity.R
    #> ├── 08_check_convergence.R
    #> └── 09_dispersion.R

The `R/00_functions.R` files contains all custom R-functions that are
defined for the project. The file h\_\*(1-4).R containts the analysis of
the four hypotheses. The `R/combine_plots.R` file combines all plots
generated for each hypothesis into one large multi-panel figure. The
`R/07_check_validity.R` script containts diagnostic code to assess the
problems within the parameter estimation of SPEEC. The
`R/08_check_convergence.R` file contains a shiny app to assess the
parameter convergence for each study. Finally `R/09_dispersion.R`
contains code to analyse the overall dispersion of the estimated
publication bias parameter.

`data/`: Data files

    #> data/
    #> ├── meta
    #> │   ├── processed
    #> │   └── raw
    #> ├── optim
    #> │   ├── processed
    #> │   └── raw
    #> └── src

The data is generally structured into three subdirectories (meta, optim,
src). The `meta` directory contains all data regarding the raw and
processed meta-analytical data. The `optim` directory contains all data
related to the parameter optimization (raw, and processed). Finally, the
`src` folder contains additional data that is needed within the Quarto
file to compile the thesis.

`scripts/`: Quarto scripts that are compiled into a reproducible report

    #> scripts/
    #> ├── 0_main.qmd
    #> ├── 1_intro.qmd
    #> ├── 2_methods.qmd
    #> ├── 3_results.qmd
    #> ├── 4_discussion.qmd
    #> ├── 5_references.qmd
    #> └── 6_appendix.qmd

## Compilation

To automatically compile the thesis, run the following bash script in
the terminal:

``` bash
./build.sh
```
