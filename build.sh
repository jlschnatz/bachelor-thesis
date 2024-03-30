#!/bin/bash

quarto render --quiet
mv _manuscript/scripts/*.pdf _manuscript
rmdir _manuscript/scripts
DIR="_manuscript/thesis_bachelor_schnatz.pdf"
mv _manuscript/0_main.pdf $DIR
rm -r _manuscript/figures

echo "Output created in $DIR"
