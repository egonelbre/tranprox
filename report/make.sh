#!/bin/bash
rm -rf build/*
cp -R *.png *.tex *.bib chapters figures appendices img build
cd build
pdflatex --shell-escape main && bibtex main && pdflatex main && pdflatex main
cp main.pdf ../main.pdf