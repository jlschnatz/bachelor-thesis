#!/bin/bash

build_dockerfile() {
    # Set default values
    path=${1:-"."}
    FROM=${2:-"eddelbuettel/r2u:20.04"}
    lock_file=${3:-"renv.lock"}
    quarto_version=${4:-"0.9.522"}

    # Base Image
    from_statement="FROM $FROM AS base"
    
    # System Requirements
    echo "Getting system requirements from $lock_file file"

    sysreqs=$(Rscript -e "cat(sort(unique(c(getsysreqs::get_sysreqs('$lock_file'), 'curl', 'gdebi-core', 'snakemake'))), sep = ' \n    ')" | sed 's/$/ \\/') 
    sysreqs="RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    "$sysreqs" \
    && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*"
    
    # Create and copy folder into analysis/

    cc_analysis="
WORKDIR /project 
COPY . /project 
RUN mkdir renv/.cache 
ENV RENV_PATHS_CACHE renv/.cache
"
    
    # Initialize renv package management
    renv_initialize="
RUN R -e 'install.packages(\"remotes\")' 
RUN R -e 'remotes::install_version(\"renv\", version = \"1.0.3\")' 
RUN R -e 'renv::restore()'
    "

    # Install Quarto
download_quarto="
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v$quarto_version/quarto-$quarto_version-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb
"

    # Install TinyTex
    install_tinytex="RUN quarto tools install tinytex"

    # Second stage
    second_stage="
FROM base
WORKDIR /project
COPY --from=base /project .
"

    # CMD
    cmd_build='CMD ["snakemake", "-s", "Snakefile", "--cores", "1"]'

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
    echo "Dockerfile available at: $path/Dockerfile"
    echo "For more information see ./README.md"
}

build_dockerfile