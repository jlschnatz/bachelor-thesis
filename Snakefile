import glob

rule process_data:
    input: 
        "R/00_functions.R", 
        "data/meta/raw/data_lindenhonekopp_raw.xlsx", 
        "data/meta/raw/metadata_lindenhonekopp.csv",
        lambda wildcards: glob.glob("data/optim/raw/*.rds")
    output: 
        "data/meta/processed/data_lindenhonekopp_proc.csv", 
        "data/optim/processed/data_optim_merged.csv", 
        "data/optim/processed/data_optim_evolution.csv.gz",
        "data/optim/processed/data_optim_loss.csv.gz",
        "data/optim/processed/data_optim_evolution_confidence.csv.gz"
    shell: "Rscript R/01_process_data.R"