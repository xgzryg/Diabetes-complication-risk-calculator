# Diabetes Vascular Complication ML Calculator

This repository contains a Shiny web calculator for individualized prediction of diabetes-related vascular complications using 11 Pareto-selected random forest models.

## Quick Start

From R:

```r
shiny::runApp(".")
```

Or from the project parent path:

```r
shiny::runApp("K:/UKB文章/06.糖尿病临床数据_TREM2_FBLN1/Shiny_DiaComplication_11Models")
```

The root directory intentionally keeps only one R entry file: `app.R`.

## Repository Layout

```text
Shiny_DiaComplication_11Models/
├── app.R                         # only root-level R entry file
├── data/
│   ├── metadata/
│   │   ├── model_manifest.rds
│   │   ├── model_manifest.csv
│   │   ├── variable_metadata.rds
│   │   └── variable_metadata.csv
│   └── examples/
│       └── example_batch_template.csv
├── models/
│   └── <outcome>/
│       ├── best_model.rds
│       ├── scale_params.rds
│       ├── best_model_features.txt
│       └── model performance/result files
├── www/                           # static assets, currently optional
├── deployment/
│   └── deploy.R
└── docs/
    ├── README.md
    └── GITHUB_STRUCTURE.md
```

## Required R Packages

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "DT",
  "randomForestSRC",
  "httr",
  "jsonlite",
  "base64enc",
  "readxl"
))
```

## Main Features

- Select one, multiple, or all 11 outcomes.
- Dynamic input form showing only the union of variables required by selected outcomes.
- Shared variables are entered once only.
- Uses outcome-specific training standardization before prediction.
- Shows probability cards, risk ranking, grouped visualizations, model performance, variable-standardization view, and risk interpretation text.
- CSV batch upload for multiple patients.
- Batch prediction summary plot.
- Download single-patient and batch prediction results.
- Download a CSV template from the web UI.
- AI Auto-Fill module for extracting required variables from uploaded images or tables using an OpenAI-compatible vision/chat API.

## AI Auto-Fill Module

The AI module is optional and session-based.

Supported upload types:

- Images: PNG, JPG, JPEG, WEBP, GIF.
- Tables: CSV, TSV, TXT, XLS, XLSX.

Workflow:

1. Select outcomes first.
2. Open `AI Auto-Fill`.
3. Enter API base URL, API key, and model name.
4. Upload a lab report image, clinical screenshot, or table.
5. Click `Build Prompt` to generate the extraction prompt.
6. Click `Run AI Extraction`.
7. Review the extracted values and evidence.
8. Click `Apply Extracted Values To Form`.
9. Return to `Risk Dashboard` and calculate risk.

Default API configuration:

- API base URL: `https://api.openai.com/v1`
- Model: `gpt-4o-mini`

Other OpenAI-compatible providers can be used if they support the `/chat/completions` endpoint and image input in `image_url` format.

Security note:

- The API key is entered in the web session only.
- The app does not write the API key to project files.
- Do not deploy with hard-coded credentials.

## Batch CSV Format

- One row per patient.
- Optional `ID` column.
- Predictor columns should use the exact variable names in `data/metadata/variable_metadata.csv`.
- The app validates missing columns before scoring.
- Use `data/examples/example_batch_template.csv` or the in-app Template CSV download button.

## Deployment

Configure rsconnect credentials outside the project, then run:

```r
source("deployment/deploy.R")
```

## Important Notes

- The deployed prediction interface uses each model object's actual `xvar.names`, which currently require 39 unique input columns across all 11 models.
- The `best_model_features.txt` files are retained as display/model-summary feature lists.
- Risk strata are relative categories based on each model's stored prediction distribution.
- This is a research and clinical decision-support calculator, not a stand-alone diagnostic tool.
- AI extraction results must be reviewed manually before prediction.
