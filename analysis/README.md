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
  outputs/
    classifiers/
      figures/
      tables/
    energy_expenditure/
      figures/
      tables/
    quality_assessment/
      figures/
      tables/
    study_characteristics/
      figures/
      tables/
```

## Requirements

Run the scripts with R from the repository root. The scripts install required R packages if they are not already available.

## Run

```bash
Rscript analysis/scripts/01_classifiers_meta.R
Rscript analysis/scripts/02_energy_expenditure.R
Rscript analysis/scripts/03_study_characteristics_figures.R
Rscript analysis/scripts/04_quadas2_figure.R
```

Each script writes to its matching folder under `analysis/outputs/` and overwrites files with the same names. The `analysis/outputs/` folder is generated locally and is not tracked by Git.

## Script Outputs

### `01_classifiers_meta.R`

Tables:
- `meta_results_all.csv`
- `meta_results_traditional.csv`
- `meta_results_nontraditional.csv`
- `pooled_within_domain.csv`
- `method_contrast_results_overall.csv`
- `method_contrast_by_domain.csv`
- `descriptives_overall.csv`
- `descriptives_by_metric.csv`
- `descriptives_by_domain.csv`
- `qc_boundary_flags.csv`

Figures:
- `forest_F1_all.png`
- `forest_Sensitivity_all.png`
- `forest_Specificity_all.png`
- `forest_*_traditional.png`
- `forest_*_nontraditional.png`
- `forest_*_bydomain_<domain>.png`

### `02_energy_expenditure.R`

Tables:
- `ee_meta_results_all.csv`
- `ee_method_contrast_results_overall.csv`

Figures:
- `forest_MAPE_all.png`

### `03_study_characteristics_figures.R`

Tables:
- `country_counts.csv`

Figures:
- `year_bar_all.png`
- `year_bar_threshold_vs_nontraditional.png`
- `year_bar_by_method.png`
- `country_bar.png`
- `country_bubble.png`
- `age_group_upset.png`
- `outcomes_upset.png`
- `devices_upset.png`
- `sample_size_by_age_group.png`
- `sample_size_and_females.png`
- `health_status_pie.png`
- `sampling_rate_pie.png`

### `04_quadas2_figure.R`

Tables:
- `quadas2_risk_of_bias_summary.csv`

Figures:
- `quadas2_risk_of_bias.png`

## Notes

- The scripts expect the input filenames listed above.
- Forest plots and pooled estimates are generated from the current CSV inputs.
