# Analysis reproduction

This folder reproduces all analysis outputs reported in the manuscript, including pooled estimates, method contrasts, forest-style plots, descriptive tables, study-characteristics figures, and the QUADAS-2 risk of bias figure.

## Folder structure

```pgsql
analysis/
  README.md
  scripts/
    01_classifiers_meta.R
    02_energy_expenditure_meta.R
    03_study_characteristics_figures.R
    04_quadas2_figure.R
  inputs/
    Data_Dictionary.pdf
    Extraction_EnergyExpenditure.csv
    Extraction_Classifiers.csv
    Quality_Assessment.csv
    Study_Characteristics.csv
  outputs/
    classifiers/
      tables/
      figures/
    energy_expenditure/
      tables/
      figures/
    study_characteristics/
      tables/
      figures/
    quality_assessment/
      tables/
      figures/
```


## Required input files

Place the input files in analysis/inputs/ in your workflow, if not already present (recommended), or edit `in_dir` in each script.

1) Classifiers meta-analysis
   - Extraction_Models_Master Sheet with Validation IDs.csv

2) Energy expenditure (association) meta-analysis
   - Extraction_Energy Expenditure Models with Validation IDs.csv

3) Study characteristics figures
   - Study_characteristics.csv

4) QUADAS-2 figure
   - Quality Assessment.csv

## Software

R (>= 4.2 recommended). The scripts install required packages if missing.

## How to run

### Step 0: Clone the repository
```bash
git clone https://github.com/miltheo/SCOPE-MOVE.git
```

```bash
cd SCOPE-MOVE
```

Option A (recommended, from R):
Open R in the project root (the folder that contains analysis/), then run:
```r
source("analysis/inputs/01_classifiers_meta.R")
source("analysis/inputs/02_energy_expenditure_meta.R")
source("analysis/inputs/03_study_characteristics_figures.R")
source("analysis/inputs/04_quadas2_figure.R")
```

Option B (command line):
```bash
Rscript analysis/inputs/01_classifiers_meta.R
Rscript analysis/inputs/02_energy_expenditure_meta.R
Rscript analysis/inputs/03_study_characteristics_figures.R
Rscript analysis/inputs/04_quadas2_figure.R
```

## Outputs produced

All scripts write outputs into analysis/outputs/ and overwrite existing files of the same name.

### 01_classifiers_meta.R

**Tables**
- `meta_results_all.csv`
- `meta_results_threshold.csv`
- `meta_results_nontraditional.csv`
- `pooled_within_domain.csv`
- `method_contrast_results_overall.csv`
- `method_contrast_by_domain.csv`
- `descriptives_overall.csv`
- `descriptives_by_metric.csv`
- `descriptives_by_domain.csv`
- `qc_boundary_flags.csv`

**Figures**
- `forest_F1_all.png`
- `forest_Sensitivity_all.png`
- `forest_Specificity_all.png`
- `forest_*_threshold.png` and `forest_*_nontraditional.png` (if enabled)
- `forest_*_bydomain_<domain>.png` (if enabled)


### 02_energy_expenditure_meta.R

**Tables**
- `ee_meta_results_all.csv`
- `ee_method_contrast_results_overall.csv`

**Figures**:
- `forest_MAPE_all.png`

### 03_study_characteristics_figures.R

**Tables**
- `country_counts.csv (optional convenience)`

**Figures**
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

### 04_quadas2_figure.R
**Tables**
- `quadas2_risk_of_bias_summary.csv`

**Figures**
- `quadas2_risk_of_bias.png`

## Notes

- Forest plots and pooled estimates are computed using the same modelling approach used in the manuscript.
- Scripts are intended to be deterministic given the same input CSVs.
