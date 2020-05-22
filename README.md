# covid-research
Datasets, replications, and analysis related to the COVID-19 pandemic.

# Repo structure

The structure of this repository is inspired by [Cookiecutter Data Science](https://drivendata.github.io/cookiecutter-data-science/).

## data

Contains all datasets.

### external

Datasets received from other people.

### interim

Intermediate-stage datasets: not in the raw, originally received or downloaded form, but not yet ready for analysis.

### processed

Fully cleaned datasets. Can be immediately loaded then visualized or analyzed.

### raw

Raw dataset downloads live here. Never modified from their original form.

## reports

Generated analyses as HTML, PDF, LaTeX, etc. This directory contains final outputs to be disseminated.

### figures

Individual plots or visualizations files to be loaded into reports (e.g. LaTeX).

## src

All scripts used to download, clean, analyze and visualize data.

### analysis

Analysis code lives here. These scripts should only load from processed data: i.e., datasets in data/processed/.

### data

These scripts download and clean code, preparing it for analysis.

#### clean

#### download