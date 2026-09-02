# Malaria Drug Resistance Analysis - IDDO Summer Placement

Analysis of malaria drug resistance patterns using surveillance data, built during a summer research placement (June–July 2026) at the **Infectious Diseases Data Observatory (IDDO)**, Big Data Institute, University of Oxford.

## Overview

This project analyses a large-scale malaria surveillance dataset (~67,000 records) to explore patterns in drug resistance markers across study sites. The pipeline covers the full workflow from raw data to a reproducible, shareable report by using cleaning, statistical analysis, geographic visualisation, and automated reporting.

No prior background in biology or malariology was required going in - this was as much a project in learning a new domain quickly as it was a technical exercise.

## What it does

- **Data cleaning**: handling missing values, inconsistent coding, and data quality issues across the raw surveillance dataset
- **Resistance marker analysis**: summarising prevalence of key resistance markers across sites and time
- **Geographic visualisation**: choropleth and bubble maps showing spatial distribution of resistance markers across study regions
- **Statistical analysis**: Spearman correlation analysis between resistance markers and other surveillance variables
- **Automated reporting**: a reproducible R Markdown pipeline that renders a full HTML report from raw data to final output

## Tech stack

- **R** / **tidyverse** for data wrangling and analysis
- **R Markdown** for the reproducible HTML report
- **ggplot2** (and mapping extensions) for geographic and statistical visualisations
- **RStudio + Git** for version control

## Notes

- Built and rendered locally in RStudio, with version control via Git/GitHub.
- Some challenges tackled along the way: file-locking issues from working out of OneDrive, R Markdown knitting errors, and network access quirks during rendering - all resolved as part of getting to a clean, reproducible pipeline.

## About

Placement supervised at IDDO, Big Data Institute, University of Oxford, summer 2026.
