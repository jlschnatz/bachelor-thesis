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
    cc_analysis="WORKDIR /project
    COPY . /project
    RUN mkdir renv/.cache
    ENV RENV_PATHS_CACHE renv/.cache"
    
    # Initialize renv package management
    renv_initialize="RUN R -e 'install.packages(\"remotes\")'
    RUN R -e 'remotes::install_version(\"renv\", version = \"1.0.3\")'
    RUN R -e 'renv::restore()'"

    # Install Quarto
    download_quarto="RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v$quarto_version/quarto-$quarto_version-linux-amd64.deb
    RUN gdebi --non-interactive quarto-linux-amd64.deb"

    # Install TinyTex
    install_tinytex="RUN quarto tools install tinytex"

    # Second stage
    second_stage="FROM base
    WORKDIR /project
    COPY --from=base /project ."

    # CMD
    cmd_build="CMD [\"./build.sh\"]"

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

# Tidy bib-file (only used references)
clean_bib() {
    # initialize variables
    folder="scripts"
    input_ref="bibliography/references.bib"
    output_ref="bibliography/tidy_references.bib"

    # Extract unique entry names from files in the folder
    entries=$(cat "$folder"/*.qmd | grep -o '@[a-zA-Z0-9_-]\+' | sed 's/@//g' | sort -u)

    output=""
    # Iterate over each entry and find the first occurrence in the bibliography
    for entry in $entries; do
        result=$(awk -v RS='\\n@' -v entry="$entry" '$0 ~ "{"entry"," {print "@" $0}' "$input_ref")
        if [ -n "$result" ]; then
            # Make the URL field lowercase
            result=$(echo "$result" | awk '{gsub(/url = \{([^}]*)\}/,tolower("&"))}1')
            output+="$result\n"
        fi
    done

    # Write the tidy references to the output file
    echo -e "$output" > "$output_ref"
}

clean_bib 

# Render Quarto Project
render_quarto() {
    # render project
    DIR="manuscript"
    AUTHOR="schnatz"
    rm -r ${DIR}
    mkdir ${DIR}

    quarto render --quiet

    # remove unnecessary folders that are rendered (fails silently)
    mv ${DIR}/scripts/*.pdf ${DIR} 2>/dev/null
    rm -r ${DIR}/scripts 2>/dev/null
    rm -r ${DIR}/figures 2>/dev/null

    # rename pdf with current date and time
    DATE=$(date +"%d-%m-%Y")
    TIME=$(date +"%H-%M")
    FILENAME="thesis-${AUTHOR}_${TIME}_${DATE}.pdf"
    mv ${DIR}/*.pdf "${DIR}/${FILENAME}"

    # echo sucess / fail information on exit
    if [ $(ls ${DIR} | wc -l) -eq 0 ]; then
        LOG_FILE=$(find scripts -name "*.log" -type f)
        echo "An error occurred in the compilation of the document, please see ${LOG_FILE}"
    else
        echo "Output created in ${DIR}/${FILENAME}"
    fi
}

render_quarto