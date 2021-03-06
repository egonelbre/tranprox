#!/bin/bash
rm -rf build/*
cp -r *.tex *.bib chapters figures appendices build
cd build
pdflatex --shell-escape main && bibtex main && pdflatex main && pdflatex main
cp main.pdf ../main.pdf