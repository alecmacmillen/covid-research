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

#### allcott

Replication and extension scripts are saved here:

- *social_distancing.Rmd*: analysis using Safegraph social distancing measures as dependent variables.
- *POIs.Rmd* (to come): analysis using Safegraph POI data as dependent variables.

### data

These scripts download and clean code, preparing it for analysis. There are two subdirectories: *download* and *clean*.

#### clean

Used for direct pulling of datasets from Github, the internet, API sources, etc. Datasets at this stage are saved to *raw* (omitted from the online version of this repository).

#### download

Used to transform datasets. There are subdirectories for each category of data (cases & deaths, demographic, mobility, political, policy responses, weather, etc.) Data transformed at this stage is saved to *data/interim*. The processing script in *src/data/clean/allcott* combines the transformed data into the final data builds, which are saved in *data/processed*.
