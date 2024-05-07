#!/bin/bash

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
    #DATE=$(date +"%d-%m-%Y")
    #TIME=$(date +"%H-%M")
    #FILENAME="thesis-${AUTHOR}_${TIME}_${DATE}.pdf"
    FILENAME="thesis-${AUTHOR}.pdf"
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