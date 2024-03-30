# Bachelor Thesis

## Project Structure

-   `R/`: R scripts for data analysis and visualization
-   `data/`: Data files
    -   `raw/`: Raw data files from Linden et al. (2021)
    -   `processed/`: Processed data files
-   `scripts/`: Quarto scripts that are compiled into a reproducible report
-   `_manuscript/`: The compiled thesis in PDF format
-   `renv/`: The renv setup for the project
-   `preregistration/`: folder containing the preregistration of the thesis
-   `_tex/`: The LaTeX files for the thesis (template, headers, etc.)

## Compilation

To automatically compile the thesis, run the following bash script in the terminal:

``` bash
./build.sh
```
