# Project Structure

This repository contains simulations and analyses of trait evolution and population stability under trait variability and heritability scenarios.
Below is a description of each folder and its contents.

---

## exploration

- Contains various Quarto notebooks and an additional README to have an overview of the analysis done

## functions_main

- Contains the core functions used throughout the project, continuously updated. The main functions is slightly different for the three scenarios of: absence of plasticity, total plasticity or for intermediate mix of the two.
- Other functions folder contain other functions necessary in plotting and simulations.
- All functions in this folder will be later sourced by the simulations scripts.

## functions_original

- Contains the commented-out *first version* of the main function, used for simulations without intraspecific variability.

## jupiter_lab

- Small python codes for short analysis of the mathematical assumption underneath the simulations.

## plots

- Stores plots as .png or .pdf generated during the analysis process.

## reports

- Contains compiled Quarto documents (PDF/HTML) summarizing the main analyses.

## server_runs

- Contains the three main scenarios and the three R scripts that generate .RDS files after large-scale simulations that cannot be run locally.
- These `.RDS` files are later used in the **exploration** stage.

---
