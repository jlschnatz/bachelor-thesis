#!/bin/bash

# render project
DIR="manuscript"
AUTHOR="schnatz"
rm -r ${DIR}
mkdir ${DIR}
quarto render --quiet

# remove unnecessary folders that are rendered
mv ${DIR}/scripts/*.pdf ${DIR}
rm -r ${DIR}/scripts
rm -r ${DIR}/figures

# rename pdf with current date and time
DATE=$(date +"%d-%m-%Y")
TIME=$(date +"%H-%M")
FILENAME="thesis-${AUTHOR}_${TIME}_${DATE}.pdf"
mv ${DIR}/*.pdf "${DIR}/${FILENAME}"

echo "Output created in ${DIR}/${FILENAME}"