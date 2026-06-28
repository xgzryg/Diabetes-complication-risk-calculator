# GitHub Structure Notes

## Design Principle

The repository root is kept minimal. Only `app.R` remains as the root-level R startup file. All data, models, deployment scripts, examples, and documentation are organized into subdirectories.

## Path Strategy

`app.R` derives all paths from:

```r
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DATA_DIR <- file.path(APP_DIR, "data")
METADATA_DIR <- file.path(DATA_DIR, "metadata")
EXAMPLES_DIR <- file.path(DATA_DIR, "examples")
MODELS_DIR <- file.path(APP_DIR, "models")
```

Therefore the app should be started from the repository root with:

```r
shiny::runApp(".")
```

or by passing the repository path to `runApp()`.

## Directory Contents

- `app.R`: single Shiny entry point.
- `data/metadata`: model manifest and variable metadata in RDS/CSV formats.
- `data/examples`: CSV example/template files.
- `models`: all outcome-specific model files and performance outputs.
- `deployment`: optional deployment helper scripts.
- `docs`: README and additional notes.
- `www`: static assets for Shiny if added later.

## GitHub Upload Notes

Model files may be large. If GitHub rejects files larger than 100 MB, use Git LFS for `*.rds` model files:

```bash
git lfs track "*.rds"
git add .gitattributes
```

Do not commit API keys, tokens, `.Renviron`, or rsconnect account secrets.
