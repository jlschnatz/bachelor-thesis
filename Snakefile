rule thesis:
    input:
        "R/functions.R",
        "data/meta/raw/data_lindenhonekopp_raw.xlsx",
        "data/meta/raw/metadata_lindenhonekopp.csv",
        "data/optim/raw/data_optim_raw.tar.gz",
        "data/optim/processed/data_optim_merged.csv",
        "data/src/plot_h1.rds",
        "data/src/plot_h2.rds",
        "data/src/plot_h3.rds",
        "data/src/plot_h4.rds",
        "tables/table_diagnostic_cormat.tex",
        "data/src/method_comparison_descr.rds",
        "figures/method_comparison.png",
        "figures/hypotheses_multipanel.png",
        "data/src/model_dispersion.rds",
        "tables/intercept_table.tex",
        "scripts/0_main.qmd",
        "scripts/1_intro.qmd",
        "scripts/2_methods.qmd",
        "scripts/3_results.qmd",
        "scripts/4_discussion.qmd",
        "scripts/5_references.qmd",
        "scripts/6_appendix.qmd",
        "tex/before-body.tex",
        "tex/header.tex",
        "_quarto.yml",
        "renv.lock",
        "README.qmd",
        "README.md"
    output: "manuscript/thesis-schnatz.pdf"
    shell: "bash ./compile_project.sh"

rule process_data:
    input:
        "R/functions.R",
        "data/meta/raw/data_lindenhonekopp_raw.xlsx",
        "data/meta/raw/metadata_lindenhonekopp.csv",
        "data/optim/raw/data_optim_raw.tar.gz"
    output:
        "data/meta/processed/data_lindenhonekopp_proc.csv",
        "data/optim/processed/data_optim_evolution.csv.gz",
        "data/optim/processed/data_optim_evolution_confidence.csv.gz",
        "data/optim/processed/data_optim_loss.csv.gz",
        "data/optim/processed/data_optim_merged.csv"
    shell: 
        """
        tar -xvf data/optim/raw/data_optim_raw.tar.gz
        Rscript R/process_data.R
        rm data/optim/raw/*.rds
        """

rule hypotheses:
    input:
        "R/functions.R",
        "data/optim/processed/data_optim_merged.csv",
        "data/meta/processed/data_lindenhonekopp_proc.csv"
    output:
        model = "data/src/model_{hypothesis}.rds",
        plot = "data/src/plot_{hypothesis}.rds",
        table = "tables/table_{hypothesis}.tex"
    shell: "Rscript R/{wildcards.hypothesis}.R"

rule combine_plots:
    input:
        "R/functions.R",
        "data/src/plot_h1.rds",
        "data/src/plot_h2.rds",
        "data/src/plot_h3.rds",
        "data/src/plot_h4.rds"
    output: "figures/hypotheses_multipanel.png"
    shell: "Rscript R/combine_plots.R"

rule check_diagnostics:
    input:
        "R/functions.R",
        "data/meta/processed/data_lindenhonekopp_proc.csv",
        "data/optim/processed/data_optim_merged.csv"
    output:
        "tables/table_diagnostic_cormat.tex",
        "data/src/method_comparison_descr.rds",
        "figures/method_comparison.png"
    shell: "Rscript R/check_validity.R"

rule sesoi_assumptions:
    input:
        "R/functions.R",
        "data/optim/processed/data_optim_merged.csv"
    output: 
        "data/src/model_dispersion.rds",
        "tables/intercept_table.tex"
    shell: "Rscript R/dispersion.R"

rule readme:
    input: "README.qmd"
    output: "README.md"
    shell: "quarto render {input}"