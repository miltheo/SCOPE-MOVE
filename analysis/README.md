# Analysis Reproduction

This folder contains the analysis inputs, scripts, and generated outputs for the SCOPE-MOVE evidence base.

## Structure

```text
analysis/
  README.md
  inputs/
    Data_Dictionary.pdf
    Extraction_Classifiers.csv
    Extraction_EnergyExpenditure.csv
    Quality_Assessment.csv
    Study_Characteristics.csv
  scripts/
    01_classifiers_meta.R
    02_energy_expenditure.R
    03_study_characteristics_figures.R
    04_quadas2_figure.R
    05_model_context.R
    06_rmse.R
    07_export_app.R
    08_publication_figures.R
    analysis_helpers.R
    reproduce_manuscript.R
  outputs/
    manuscript/
      Figures/Main/
      Figures/Supplementary/
      Tables/
```

## Requirements

Run the scripts with R from the repository root. The scripts install required R packages if they are not already available. Rebuilding the app data bundle also requires Node.js.

## Run

```bash
Rscript analysis/scripts/reproduce_manuscript.R
node app/build-data.js
```

The workflow writes under `analysis/outputs/manuscript/` and overwrites files with the same names. The `analysis/outputs/` folder is generated locally and is not tracked by Git. Numbered data-derived manuscript figures and combined result tables are written to `analysis/outputs/manuscript/`.

## Script Outputs

The workflow writes the current pooled estimates, method contrasts, heterogeneity, variance sensitivity, validation-level data, quality-assessment summary and figure captions to `analysis/outputs/manuscript/Tables/`. Figures are exported once as 600-dpi PNGs in `Figures/Main/` or `Figures/Supplementary/`. No fitted-model files are saved.

## Notes

- The scripts expect the input filenames listed above.
- Forest plots and pooled estimates are generated from the current CSV inputs.
