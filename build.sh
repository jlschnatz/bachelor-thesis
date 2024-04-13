#!/bin/bash

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