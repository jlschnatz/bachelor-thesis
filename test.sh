#!/bin/bash

# Define a function to build Dockerfile
build_dockerfile() {
 # Define default values
local path="${1:-.}"
local FROM="${2:-eddelbuettel/r2u:20.04}"
local lock_file="${3:-renv.lock}"
local quarto_version="${4:-0.9.522}"

# Base Image
local from_statement="FROM $FROM AS base"

# System Requirements
local sysreqs=$(Rscript --no-init-file --no-environ -e "suppressPackageStartupMessages({library(renv); cat(glue::glue_collapse(sort(unique(c(getsysreqs::get_sysreqs('$lock_file'), 'curl', 'gdebi-core', 'snakemake'))), sep = ' \\\n    '))})")   
RUN apt-get update -qq && \\
  apt-get install -y --no-install-recommends \\
    $sysreqs \\
 && \\
  apt-get clean && \\
  rm -rf /var/lib/apt/lists/*
EOF


    # Create and copy folder into analysis/
    local cc_analysis=$(cat <<EOF
WORKDIR /project
COPY . /project
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache
EOF
    )

    # Initialize renv package management
    local renv_initialize=$(cat <<EOF
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.3")'
RUN R -e 'renv::restore() > /dev/null 2>&1'
EOF
    )

    # Install Quarto
    local download_quarto=$(cat <<EOF
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v$quarto_version/quarto-$quarto_version-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb
EOF
    )

    # Install TinyTex
    local install_tinytex="RUN quarto tools install tinytex"

    # Second stage
    local second_stage=$(cat <<EOF
FROM base
WORKDIR /project
COPY --from=base /project .
EOF
    )

    # CMD
    local cmd_build='CMD ["./build.sh"]'

    # Write to Dockerfile
    cat <<EOF > "$path/Dockerfile"
$from_statement
$sysreqs
$cc_analysis
$renv_initialize
$download_quarto
$install_tinytex
$second_stage
$cmd_build
EOF

    echo "Dockerfile created successfully! 🐳"
    echo "Dockerfile available at: $(pwd)/Dockerfile"
    echo "For more information see ./README.md"
}

# Call the function with default arguments
build_dockerfile "$@"


