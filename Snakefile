# Rule ALL
rule ALL:
    input:
        "tables/h1_table.tex",
        "tables/h2_table.tex",
        "tables/h2_table_comparison.tex",
        "tables/h3_table.tex",
        "tables/h4_table.tex",
        "figures/h1_scatter.png",
        "figures/h2_scatter.png",
        "figures/h3_scatter.png",
        "figures/h3_tost_tnull.png",
        "figures/h4_raincloud.png",
        "_manuscript/thesis_bachelor_schnatz.pdf"

# Process Meta-Analytical Data
rule PROCESS_DATA:
    input:
        "data/meta/raw/data_lindenhonekopp_raw.xlsx",
        "data/meta/raw/metadata_lindenhonekopp.csv"
        "data/optim/raw/{filename}"
    output:
        "data/meta/processed/data_lindenhonekopp_proc.csv",
        "data/optim/processed/data_optim_evoultion_confidence.csv.gz",
        "data/optim/processed/optim_evolution.csv.gz",
        "data/optim/processed/data_optim_loss.csv.gz",
        "data/optim/processed/data_optim_merged.csv"
    shell: 
        "Rscript R/01_process_data.R"

rule RUN_HYPOTHESES:
    input:
        "data/processed/data_lindenhonekopp_proc.csv"
    output:
        table = "tables/{hypothesis}_table.tex",
        figure = "figures/{hypothesis}_scatter.png"
    shell:
        "Rscript R/{wildcards.hypothesis}.R"

rule BUILD_THESIS:
    input: 
        #"tables/h1_table.tex",
        #"tables/h2_table.tex",
        #"tables/h2_table_comparison.tex",
        #"tables/h3_table.tex",
        #"tables/h4_table.tex",
        #"figures/h1_scatter.png",
        #"figures/h2_scatter.png",
        #"figures/h3_scatter.png",
        #"figures/h3_tost_tnull.png",
        #"figures/h4_raincloud.png",
        "data/processed/data_lindenhonekopp_proc.csv"
    output: 
        "_manuscript/thesis_bachelor_schnatz.pdf"
    shell:
        "bash ./build.sh"