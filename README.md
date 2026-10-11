# EHC-QOL-prediction

Analysis code for the study **"Health Checkup Data to Predict Poor Quality of Life, Severe Disability, or Death in Older Adults"** (prognostic study in the Sukagawa Study, Japan; reported following TRIPOD+AI).

The code develops ridge logistic regression models from annual health checkup data of adults aged 75 years or older (fiscal year [FY] 2020), validates them temporally in later checkup participants (FY2022 and FY2023), and produces all tables and figures of the article and its online supplement.

## Data availability

Individual participant data are **not included** and cannot be shared: they are linked municipal records (health checkup records, resident registry, and long-term care insurance registry) provided under an agreement with Sukagawa City. The code is published so that the analysis can be inspected and reproduced by those with authorized access to the data. The coefficients of the final models are reported in the online supplement of the article.

## Structure

```
code/analysis/
  00_setup.R                  packages, folder paths, random seed, number of bootstrap samples
  00_build_dataset.R          raw CSV files -> cleaned data set
  01_build_analysis_sets.R    development, temporal validation, and sensitivity-analysis sets
  01b_check_data.R            pre-analysis data checks
  02_descriptive.R            participant characteristics, predictor definitions, flow diagram data
  03_development_ridge.R      model development and bootstrap internal validation (500 samples)
  04_external_validation.R    temporal validation and identification of high-risk individuals
  05_sensitivity.R            sensitivity analyses (inverse probability weighting, reclassification, thresholds, shrinkage)
  06_missing_comparison.R     participants with known vs unknown outcomes
  07_diagnostics.R            internal diagnostics
  08_by_sex.R                 results by sex
  09_recalibration.R          logistic recalibration in temporal validation
  99_RENDER_outputs.R         all tables (Word) and figures (PNG/PDF)
  run_all.R                   runs the whole pipeline in order
  R/                          helper functions (data, models, metrics, bootstrap, flow diagram)
```

Code comments are written in Japanese.

## How to run

Run from the project root, with the data placed in `data/raw/` (not included):

```r
source("code/analysis/run_all.R", encoding = "UTF-8")
```

`00_setup.R` checks that the working directory is the project root by looking for `protocol/protocol.md`; create this file (it can be empty) when running outside the original project folder. Outputs are written to `data/processed/`, `output/derived/`, `output/tables/`, and `output/figures/`. The full run takes about 2 hours on 20 cores; set `TEST_MODE <- TRUE` in `run_all.R` for a quick check with fewer bootstrap samples. The random seed is 20260921.

## Software

R 4.6.1 with glmnet 5.0, pROC 1.19.1, WeightedROC 2026.8.27, pmsampsize 1.1.3, tidyverse 2.0.0, gtsummary 2.6.1, flextable 0.10.1, officer 0.7.6, ggplot2 4.0.3, patchwork 1.3.2, consort 1.2.3, and summarytools 1.1.5; also readxl, ggsci, ragg, and scales.
