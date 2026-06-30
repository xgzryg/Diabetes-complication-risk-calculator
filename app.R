library(shiny)
library(ggplot2)
library(DT)
library(randomForestSRC)
library(httr)
library(jsonlite)
library(base64enc)
library(readxl)

options(shiny.maxRequestSize = 50 * 1024^2)

APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DATA_DIR <- file.path(APP_DIR, "data")
METADATA_DIR <- file.path(DATA_DIR, "metadata")
EXAMPLES_DIR <- file.path(DATA_DIR, "examples")
MODELS_DIR <- file.path(APP_DIR, "models")

manifest <- readRDS(file.path(METADATA_DIR, "model_manifest.rds"))
var_meta <- readRDS(file.path(METADATA_DIR, "variable_metadata.rds"))

manifest$features_list <- strsplit(manifest$features, ";", fixed = TRUE)
var_meta$input_id <- paste0("var_", gsub("[^A-Za-z0-9_]", "_", var_meta$variable))

risk_palette <- c(
  Low = "#2ca25f",
  Moderate = "#f39c12",
  High = "#de2d26"
)

load_one_model <- function(outcome_id) {
  d <- file.path(MODELS_DIR, outcome_id)
  pred_path <- file.path(d, "best_model_predictions.txt")
  pred_ref <- if (file.exists(pred_path)) {
    tryCatch(read.table(pred_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
  } else NULL
  ref_prob <- if (!is.null(pred_ref) && "PredictedProb" %in% names(pred_ref)) {
    suppressWarnings(as.numeric(pred_ref$PredictedProb))
  } else numeric(0)
  ref_prob <- ref_prob[is.finite(ref_prob)]
  thresholds <- if (length(ref_prob) >= 20) {
    as.numeric(quantile(ref_prob, probs = c(0.25, 0.75), na.rm = TRUE))
  } else c(0.33, 0.67)
  list(
    model = model_obj <- readRDS(file.path(d, "best_model.rds")),
    scale_params = readRDS(file.path(d, "scale_params.rds")),
    features = model_obj$xvar.names,
    display_features = readLines(file.path(d, "best_model_features.txt"), warn = FALSE),
    thresholds = thresholds,
    ref_prob = ref_prob
  )
}

model_bank <- setNames(lapply(manifest$outcome_id, load_one_model), manifest$outcome_id)

label_for <- function(v) {
  x <- var_meta$label[match(v, var_meta$variable)]
  ifelse(is.na(x), v, x)
}

unit_for <- function(v) {
  x <- var_meta$unit[match(v, var_meta$variable)]
  ifelse(is.na(x), "", x)
}

fmt_prob <- function(x) sprintf("%.1f%%", 100 * x)

risk_text <- function(class) {
  switch(class,
    Low = "The predicted probability is in the lower reference stratum for this model. Continue routine evidence-based vascular risk management and interpret with the full clinical profile.",
    Moderate = "The predicted probability is in the intermediate reference stratum. Consider closer risk-factor review, medication optimization, and targeted follow-up depending on clinical context.",
    High = "The predicted probability is in the upper reference stratum. This result may support more intensive evaluation, prevention planning, and monitoring, but it should not replace clinician judgment.",
    "Risk stratum is unavailable because probability could not be classified."
  )
}

risk_class <- function(prob, thresholds) {
  if (is.na(prob)) return("Unknown")
  if (prob < thresholds[1]) return("Low")
  if (prob < thresholds[2]) return("Moderate")
  "High"
}

predict_one <- function(outcome_id, raw_values) {
  obj <- model_bank[[outcome_id]]
  features <- obj$features
  x <- as.data.frame(as.list(raw_values[features]), check.names = FALSE)
  names(x) <- features
  for (v in features) x[[v]] <- as.numeric(x[[v]])

  center <- obj$scale_params$center[features]
  scale <- obj$scale_params$scale[features]
  center[is.na(center)] <- 0
  scale[is.na(scale) | scale == 0] <- 1

  x_scaled <- sweep(as.matrix(x), 2, center, "-")
  x_scaled <- sweep(x_scaled, 2, scale, "/")
  x_scaled[!is.finite(x_scaled)] <- 0
  x_scaled <- as.data.frame(x_scaled, check.names = FALSE)

  pr <- predict(obj$model, newdata = x_scaled)$predicted
  prob <- if (is.matrix(pr) || is.data.frame(pr)) {
    cn <- colnames(pr)
    if (!is.null(cn) && "1" %in% cn) as.numeric(pr[, "1"])
    else as.numeric(pr[, ncol(pr)])
  } else {
    as.numeric(pr)
  }
  prob <- max(0, min(1, prob[1]))
  class <- risk_class(prob, obj$thresholds)

  std_values <- as.numeric(x_scaled[1, features, drop = TRUE])
  explain_df <- data.frame(
    Variable = label_for(features),
    Feature = features,
    RawValue = as.numeric(x[1, features, drop = TRUE]),
    Unit = unit_for(features),
    StandardizedValue = std_values,
    AbsStandardizedValue = abs(std_values),
    stringsAsFactors = FALSE
  )
  explain_df <- explain_df[order(explain_df$AbsStandardizedValue, decreasing = TRUE), ]

  list(prob = prob, class = class, explain = explain_df)
}

predict_values_for_outcomes <- function(raw_values, outcomes) {
  rows <- lapply(outcomes, function(o) {
    res <- predict_one(o, raw_values)
    info <- manifest[match(o, manifest$outcome_id), ]
    data.frame(
      outcome_id = o,
      Outcome = info$outcome_label,
      Group = info$group,
      Probability = res$prob,
      ProbabilityPercent = round(100 * res$prob, 3),
      Risk = res$class,
      AUC = info$auc,
      Brier = info$brier,
      N_Features = info$n_features,
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, rows)
  ans <- ans[order(ans$Probability, decreasing = TRUE), ]
  rownames(ans) <- NULL
  ans
}


read_uploaded_table_text <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    dat <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else if (ext %in% c("xls", "xlsx")) {
    dat <- as.data.frame(readxl::read_excel(path), check.names = FALSE)
  } else if (ext %in% c("txt", "tsv")) {
    dat <- read.table(path, sep = "\t", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported table type. Please upload CSV, TSV, XLS, or XLSX.")
  }
  utils::capture.output(print(utils::head(dat, 30), row.names = FALSE)) |> paste(collapse = "\n")
}

make_ai_prompt <- function(target_vars, metadata) {
  mm <- metadata[match(target_vars, metadata$variable), ]
  var_lines <- paste0(
    "- ", mm$variable, " | label: ", mm$label,
    ifelse(nchar(mm$unit) > 0, paste0(" | unit: ", mm$unit), ""),
    " | expected numeric range approximately: ", mm$min, " to ", mm$max
  )
  paste(
    "You are a clinical data extraction assistant for a diabeticvascular complication prediction calculator.",
    "Extract the following model variables from the uploaded medical image, lab report, clinical note, or table.",
    "Return ONLY valid JSON. Do not include markdown fences. Do not explain.",
    "JSON schema:",
    '{"values":{"variable_name": numeric_or_null}, "evidence":{"variable_name":"short source text or table cell"}, "warnings":["short warning"]}',
    "Rules:",
    "1. Use the exact variable names listed below as JSON keys.",
    "2. Return numeric values only. If a value is missing or uncertain, use null.",
    "3. Do not invent values.",
    "4. If units differ, convert to the requested unit when safe; otherwise return null and add a warning.",
    "5. For smoke and drink indicators, use 1 for current yes/present and 0 for no/not current when explicitly available; otherwise null.",
    "6. For TREM2_per100, extract the already transformed TREM2_per100 value if present; do not infer it from raw TREM2 unless clearly defined.",
    "Variables to extract:",
    paste(var_lines, collapse = "\n"),
    sep = "\n"
  )
}

extract_json_text <- function(x) {
  x <- trimws(x)
  x <- sub("^```json\\s*", "", x)
  x <- sub("^```\\s*", "", x)
  x <- sub("\\s*```$", "", x)
  start <- regexpr("\\{", x)
  end <- max(gregexpr("\\}", x)[[1]])
  if (start[1] > 0 && end > start[1]) substr(x, start[1], end) else x
}

call_openai_compatible_vision <- function(api_base, api_key, model, prompt, file_path = NULL, file_name = NULL, table_text = NULL) {
  api_base <- sub("/+$", "", api_base)
  url <- if (grepl("/chat/completions$", api_base)) api_base else paste0(api_base, "/chat/completions")
  content <- list(list(type = "text", text = prompt))
  if (!is.null(table_text) && nzchar(table_text)) {
    content <- append(content, list(list(type = "text", text = paste("Uploaded table preview:\n", table_text))))
  }
  if (!is.null(file_path) && file.exists(file_path)) {
    ext <- tolower(tools::file_ext(file_name %||% file_path))
    mime <- switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg", png = "image/png", webp = "image/webp", gif = "image/gif", "application/octet-stream")
    if (grepl("^image/", mime)) {
      b64 <- base64enc::base64encode(file_path)
      content <- append(content, list(list(type = "image_url", image_url = list(url = paste0("data:", mime, ";base64,", b64)))))
    }
  }
  body <- list(
    model = model,
    messages = list(list(role = "user", content = content)),
    temperature = 0,
    response_format = list(type = "json_object")
  )
  resp <- httr::POST(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw",
    httr::timeout(120)
  )
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (httr::status_code(resp) >= 300) stop(paste("AI API error", httr::status_code(resp), txt))
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  parsed$choices[[1]]$message$content
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

custom_css <- "
:root {
  --primary: #1f3a5f;
  --primary2: #315f8f;
  --accent: #13a8a8;
  --danger: #de2d26;
  --warning: #f39c12;
  --success: #2ca25f;
  --muted: #6c757d;
  --bg: #f4f7fb;
  --card: #ffffff;
  --line: #e7edf5;
  --shadow: 0 10px 28px rgba(31, 58, 95, 0.12);
}
body { background: var(--bg); color: #23364d; font-family: 'Inter', 'Segoe UI', Arial, sans-serif; }
.hero {
  background: radial-gradient(circle at top left, rgba(19,168,168,0.35), transparent 34%), linear-gradient(135deg, #14243d, #1f3a5f 52%, #315f8f);
  color: white; padding: 28px 34px; border-radius: 0 0 28px 28px; margin: 0 -15px 22px -15px;
  box-shadow: var(--shadow);
}
.hero h1 { font-weight: 800; letter-spacing: -0.5px; margin: 0 0 8px 0; }
.hero p { opacity: 0.92; font-size: 16px; margin: 0; max-width: 980px; }
.hero-badge { display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.14); border: 1px solid rgba(255,255,255,0.22); border-radius: 999px; margin: 14px 8px 0 0; font-size: 13px; }
.side-card, .main-card, .metric-card {
  background: var(--card); border: 1px solid var(--line); border-radius: 18px; padding: 18px; box-shadow: var(--shadow); margin-bottom: 18px;
}
.side-card h4, .main-card h4 { margin-top: 0; font-weight: 800; color: var(--primary); }
.section-title { font-size: 13px; text-transform: uppercase; letter-spacing: 0.09em; color: var(--muted); font-weight: 800; margin: 16px 0 8px 0; }
.btn-primary, .btn-default { border-radius: 12px; font-weight: 700; }
.btn-primary { background: linear-gradient(135deg, var(--accent), var(--primary2)); border: none; }
.btn-primary:hover { filter: brightness(0.95); }
.form-control { border-radius: 10px; border-color: #d9e3ef; }
.checkbox label { line-height: 1.25; }
.var-chip { display: inline-block; margin: 3px 4px 3px 0; padding: 5px 10px; border-radius: 999px; background: #eef6ff; border: 1px solid #d7eaff; font-size: 12px; color: #254b72; }
.risk-card { color: white; border-radius: 18px; padding: 18px; margin-bottom: 14px; box-shadow: var(--shadow); min-height: 132px; }
.risk-card h3 { margin: 4px 0 8px 0; font-weight: 850; font-size: 34px; }
.risk-card .name { font-size: 15px; font-weight: 750; min-height: 40px; }
.risk-card .tag { display: inline-block; padding: 4px 10px; background: rgba(255,255,255,0.18); border-radius: 999px; font-size: 12px; }
.stat-pill { display: inline-block; margin: 5px 8px 5px 0; padding: 6px 10px; border-radius: 999px; background: #eef6ff; color: #315f8f; font-weight: 700; font-size: 12px; }
.help-note { color: var(--muted); font-size: 13px; line-height: 1.55; }
.nav-tabs > li > a { border-radius: 12px 12px 0 0; font-weight: 700; color: var(--primary); }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { background: var(--primary); color: white; }
.input-panel { max-height: 72vh; overflow-y: auto; padding-right: 5px; }
.footer { color: #789; text-align: center; font-size: 12px; padding: 18px 0 24px 0; }
.cover-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 14px; margin-bottom: 18px; }
.cover-tile { background: linear-gradient(135deg, #ffffff, #f7fbff); border: 1px solid var(--line); border-radius: 18px; padding: 18px; box-shadow: var(--shadow); min-height: 128px; }
.cover-tile .icon { font-size: 28px; color: var(--accent); margin-bottom: 12px; }
.cover-tile .big { font-size: 30px; font-weight: 850; color: var(--primary); line-height: 1; }
.cover-tile .label { color: var(--muted); font-weight: 700; margin-top: 8px; }
.explain-box { border-left: 5px solid var(--accent); background: #f7fbff; border-radius: 14px; padding: 14px 16px; margin-top: 12px; }
.workflow-step { display: flex; align-items: flex-start; gap: 12px; margin: 12px 0; }
.workflow-step .num { background: var(--primary); color: white; width: 28px; height: 28px; border-radius: 50%; text-align: center; line-height: 28px; font-weight: 800; flex: none; }
@media (max-width: 1100px) { .cover-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
.ai-prompt { font-family: 'Cascadia Code', Consolas, monospace; font-size: 12px; }
.ai-result { background: #0f172a; color: #e2e8f0; border-radius: 14px; padding: 14px; max-height: 360px; overflow-y: auto; font-family: 'Cascadia Code', Consolas, monospace; font-size: 12px; }
.map-ok { color: #238b45; font-weight: 800; }
.map-miss { color: #de2d26; font-weight: 800; }
@media (max-width: 700px) { .cover-grid { grid-template-columns: 1fr; } }
"

ui <- fluidPage(
  tags$head(
    tags$title("DiaComplication ML Calculator"),
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css")
  ),
  div(class = "hero",
      h1(tags$i(class = "fa-solid fa-heart-pulse"), " DiabeticVascular Complication ML Calculator"),
      p("A multi-outcome web-based calculator for individualized prediction of diabetes-related vascular complications using 11 Pareto-selected random forest models."),
      span(class = "hero-badge", "11 outcomes"),
      span(class = "hero-badge", "39 model input columns"),
      span(class = "hero-badge", "Dynamic form"),
      span(class = "hero-badge", "No repeated input")
  ),
  sidebarLayout(
    sidebarPanel(width = 4,
      div(class = "side-card",
          h4(tags$i(class = "fa-solid fa-list-check"), " Select Outcomes"),
          div(class = "help-note", "Choose one, several, or all outcomes. The input form below will automatically show the union of variables required by the selected models."),
          br(),
          fluidRow(
            column(6, actionButton("select_all", "Select All", icon = icon("check-double"), class = "btn-primary", width = "100%")),
            column(6, actionButton("select_none", "Clear", icon = icon("eraser"), width = "100%"))
          ),
          br(),
          checkboxGroupInput(
            "outcomes", NULL,
            choices = setNames(manifest$outcome_id, manifest$outcome_label),
            selected = c("any_vascular", "macro_vascular", "micro_vascular")
          )
      ),
      div(class = "side-card",
          h4(tags$i(class = "fa-solid fa-wand-magic-sparkles"), " AI Auto-Fill"),
          div(class = "help-note", "Optional: use an OpenAI-compatible vision API to extract variables from a lab image, clinical table, or report."),
          br(),
          actionButton("go_ai_tab", "Open AI Extraction", icon = icon("robot"), width = "100%")
      ),
      div(class = "side-card input-panel",
          h4(tags$i(class = "fa-solid fa-keyboard"), " Patient Inputs"),
          uiOutput("needed_summary"),
          uiOutput("dynamic_inputs"),
          hr(),
          actionButton("predict_btn", "Calculate Risk", icon = icon("calculator"), class = "btn-primary", width = "100%")
      )
    ),
    mainPanel(width = 8,
      tabsetPanel(id = "tabs",
        tabPanel("Overview",
          br(),
          div(class = "cover-grid",
              div(class = "cover-tile", div(class = "icon", tags$i(class = "fa-solid fa-layer-group")), div(class = "big", "11"), div(class = "label", "vascular complication outcomes")),
              div(class = "cover-tile", div(class = "icon", tags$i(class = "fa-solid fa-keyboard")), div(class = "big", "39"), div(class = "label", "model input columns, de-duplicated on screen")),
              div(class = "cover-tile", div(class = "icon", tags$i(class = "fa-solid fa-shuffle")), div(class = "big", "1x"), div(class = "label", "shared variables entered once")),
              div(class = "cover-tile", div(class = "icon", tags$i(class = "fa-solid fa-file-csv")), div(class = "big", "CSV"), div(class = "label", "single-patient and batch prediction"))
          ),
          fluidRow(
            column(7,
              div(class = "main-card",
                h4(tags$i(class = "fa-solid fa-diagram-project"), " Calculator Workflow"),
                div(class = "workflow-step", div(class = "num", "1"), div(strong("Select outcomes"), br(), "Choose one, multiple, or all diabeticvascular endpoints.")),
                div(class = "workflow-step", div(class = "num", "2"), div(strong("Enter only required variables"), br(), "The form uses the union of variables across selected models, so repeated predictors appear once.")),
                div(class = "workflow-step", div(class = "num", "3"), div(strong("Model-specific standardization"), br(), "Each model applies its own training-set center and scale parameters before prediction.")),
                div(class = "workflow-step", div(class = "num", "4"), div(strong("Interpret risk with context"), br(), "Output probabilities, relative strata, charts, and downloadable tables are provided for clinical review."))
              )
            ),
            column(5,
              div(class = "main-card",
                h4(tags$i(class = "fa-solid fa-stethoscope"), " Scope"),
                p(class = "help-note", "This application is designed as a research-grade deployment interface for Pareto-selected machine-learning models in patients with diabetes."),
                p(class = "help-note", "It supports individualized prediction and batch scoring for macrovascular and microvascular complication endpoints."),
                div(class = "explain-box", strong("Important: "), "Predictions support, but do not replace, clinical assessment. Risk strata are model-relative categories derived from stored prediction distributions.")
              )
            )
          )
        ),
        tabPanel("Risk Dashboard",
          br(),
          uiOutput("risk_cards"),
          fluidRow(
            column(7, div(class = "main-card", h4(tags$i(class = "fa-solid fa-chart-column"), " Predicted Risk Across Selected Outcomes"), plotOutput("risk_plot", height = "390px"))),
            column(5, div(class = "main-card", h4(tags$i(class = "fa-solid fa-gauge-high"), " Risk Spectrum"), plotOutput("gauge_plot", height = "390px")))
          ),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-table"), " Prediction Table"), DTOutput("risk_table"), br(), downloadButton("download_predictions", "Download CSV")),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-comment-medical"), " Risk Interpretation"), uiOutput("risk_interpretation"))
        ),
        tabPanel("Variable View",
          br(),
          fluidRow(
            column(6, div(class = "main-card", h4(tags$i(class = "fa-solid fa-flask"), " Required Variables"), uiOutput("var_chips"))),
            column(6, div(class = "main-card", h4(tags$i(class = "fa-solid fa-circle-info"), " Input Design"),
              p(class = "help-note", "Variables shared by multiple models are entered only once. Numeric defaults and ranges are derived from the imputed analysis dataset where available."),
              p(class = "help-note", "Smoking history and alcohol consumption history are kept as numeric recorded values because the trained model used numeric columns, not category labels.")))
          ),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-scale-balanced"), " Top Standardized Deviations"),
              p(class = "help-note", "For random forest models, this is not a linear coefficient contribution. It shows which entered values deviate most from the model training center among the selected model features."),
              selectInput("explain_outcome", "Outcome for variable view", choices = setNames(manifest$outcome_id, manifest$outcome_label)),
              plotOutput("deviation_plot", height = "420px"),
              DTOutput("deviation_table"))
        ),
        tabPanel("AI Auto-Fill",
          br(),
          fluidRow(
            column(5,
              div(class = "main-card",
                  h4(tags$i(class = "fa-solid fa-key"), " API Settings"),
                  textInput("ai_api_base", "API base URL", value = "https://api.openai.com/v1"),
                  passwordInput("ai_api_key", "API key", value = ""),
                  textInput("ai_model", "Vision/chat model", value = "gpt-4o-mini"),
                  fileInput("ai_file", "Upload image or table", accept = c(".png", ".jpg", ".jpeg", ".webp", ".gif", ".csv", ".tsv", ".txt", ".xls", ".xlsx")),
                  checkboxInput("ai_selected_only", "Extract variables for currently selected outcomes only", value = TRUE),
                  actionButton("build_prompt", "Build Prompt", icon = icon("pen-to-square"), width = "49%"),
                  actionButton("run_ai_extract", "Run AI Extraction", icon = icon("wand-magic-sparkles"), class = "btn-primary", width = "49%"),
                  br(), br(),
                  actionButton("apply_ai_values", "Apply Extracted Values To Form", icon = icon("check"), class = "btn-primary", width = "100%"),
                  br(), br(),
                  div(class = "help-note", "The API key is used only during this session and is not written to app files. Always review extracted values before prediction."))
            ),
            column(7,
              div(class = "main-card",
                  h4(tags$i(class = "fa-solid fa-terminal"), " Extraction Prompt"),
                  textAreaInput("ai_prompt", NULL, value = "", rows = 16, width = "100%")),
              div(class = "main-card",
                  h4(tags$i(class = "fa-solid fa-code"), " AI JSON Response"),
                  uiOutput("ai_status"),
                  verbatimTextOutput("ai_raw_response"))
            )
          ),
          div(class = "main-card",
              h4(tags$i(class = "fa-solid fa-clipboard-check"), " Extracted Variable Mapping"),
              DTOutput("ai_mapping_table"))
        ),
        tabPanel("Batch Prediction",
          br(),
          fluidRow(
            column(5,
              div(class = "main-card",
                  h4(tags$i(class = "fa-solid fa-upload"), " Upload Patient CSV"),
                  p(class = "help-note", "Upload a CSV file with one row per patient. Columns should use the variable names in the template. If an ID column is present, it will be retained; otherwise row numbers are used."),
                  fileInput("batch_file", "CSV file", accept = c(".csv")),
                  checkboxInput("batch_selected_only", "Use currently selected outcomes", value = TRUE),
                  fluidRow(
                    column(6, downloadButton("download_template", "Template CSV", width = "100%")),
                    column(6, actionButton("run_batch", "Run Batch", icon = icon("gears"), class = "btn-primary", width = "100%"))
                  ),
                  br(),
                  div(class = "help-note", "Tip: for web deployment, keep uploaded files reasonably small. The app validates missing columns before scoring."))
            ),
            column(7,
              div(class = "main-card",
                  h4(tags$i(class = "fa-solid fa-chart-simple"), " Batch Summary"),
                  uiOutput("batch_summary"),
                  plotOutput("batch_plot", height = "310px"))
            )
          ),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-table-list"), " Batch Results"), DTOutput("batch_table"), br(), downloadButton("download_batch", "Download Batch Results"))
        ),
        tabPanel("Model Information",
          br(),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-microchip"), " Model Inventory"), DTOutput("model_table")),
          div(class = "main-card", h4(tags$i(class = "fa-solid fa-book-medical"), " Clinical Notes"),
              tags$ul(
                tags$li("The calculator is intended for research and clinical decision support, not as a stand-alone diagnostic tool."),
                tags$li("Each outcome uses its own Pareto-selected best model and its own training-set standardization parameters."),
                tags$li("Risk categories are relative strata based on the model's stored prediction distribution: lower quartile, interquartile range, and upper quartile."),
                tags$li("All model inputs should be measured before prediction and interpreted in the clinical context of diabetes management.")
              ))
        ),
        tabPanel("Instructions",
          br(),
          div(class = "main-card",
              h4(tags$i(class = "fa-solid fa-route"), " How To Use"),
              tags$ol(
                tags$li("Select one or more vascular outcomes in the left panel."),
                tags$li("Fill in the automatically displayed variables. Shared variables only need to be entered once."),
                tags$li("Click Calculate Risk."),
                tags$li("Review the dashboard, risk ranking table, and variable view."),
                tags$li("Download the predictions as a CSV file if needed.")
              ),
              hr(),
              h4("Included Outcomes"),
              tags$div(lapply(seq_len(nrow(manifest)), function(i) span(class = "var-chip", manifest$outcome_label[i])))
          )
        )
      )
    )
  ),
  div(class = "footer", "Built with Shiny and randomForestSRC | Diabeticvascular complication machine-learning calculator")
)

server <- function(input, output, session) {
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "outcomes", selected = manifest$outcome_id)
  })
  observeEvent(input$select_none, {
    updateCheckboxGroupInput(session, "outcomes", selected = character(0))
  })
  observeEvent(input$go_ai_tab, {
    updateTabsetPanel(session, "tabs", selected = "AI Auto-Fill")
  })

  selected_outcomes <- reactive({
    x <- input$outcomes
    if (is.null(x)) character(0) else x
  })

  required_vars <- reactive({
    outs <- selected_outcomes()
    if (length(outs) == 0) return(character(0))
    unique(unlist(manifest$features_list[match(outs, manifest$outcome_id)]))
  })

  output$needed_summary <- renderUI({
    vars <- required_vars()
    div(
      span(class = "stat-pill", paste(length(selected_outcomes()), "outcome(s) selected")),
      span(class = "stat-pill", paste(length(vars), "unique variable(s) required"))
    )
  })

  output$dynamic_inputs <- renderUI({
    vars <- required_vars()
    if (length(vars) == 0) {
      return(div(class = "help-note", "Please select at least one outcome."))
    }
    m <- var_meta[match(vars, var_meta$variable), ]
    m <- m[order(match(m$category, c("Demographics", "Anthropometrics", "Vital signs", "Lifestyle", "Protein biomarker", "Laboratory biomarkers")), m$label), ]
    cats <- unique(m$category)
    tagList(lapply(cats, function(cat) {
      mm <- m[m$category == cat, ]
      tagList(
        div(class = "section-title", cat),
        lapply(seq_len(nrow(mm)), function(i) {
          row <- mm[i, ]
          lab <- paste0(row$label, ifelse(nchar(row$unit) > 0, paste0(" (", row$unit, ")"), ""))
          numericInput(row$input_id, lab, value = row$default, min = row$min, max = row$max, step = row$step, width = "100%")
        })
      )
    }))
  })

  raw_values <- reactive({
    vars <- required_vars()
    vals <- setNames(rep(NA_real_, length(vars)), vars)
    for (v in vars) {
      id <- var_meta$input_id[match(v, var_meta$variable)]
      vals[[v]] <- suppressWarnings(as.numeric(input[[id]]))
    }
    vals
  })

  predictions <- eventReactive(input$predict_btn, {
    outs <- selected_outcomes()
    shiny::validate(shiny::need(length(outs) > 0, "Select at least one outcome."))
    vals <- raw_values()
    shiny::validate(shiny::need(all(is.finite(vals)), "Please complete all required numeric inputs."))

    predict_values_for_outcomes(vals, outs)
  }, ignoreInit = FALSE)

  explain_selected <- reactive({
    vals <- raw_values()
    o <- input$explain_outcome
    if (is.null(o) || !(o %in% selected_outcomes())) {
      o <- selected_outcomes()[1]
    }
    shiny::validate(shiny::need(length(o) == 1 && !is.na(o), "Select an outcome."))
    shiny::validate(shiny::need(all(is.finite(vals)), "Please complete all required numeric inputs."))
    predict_one(o, vals)$explain
  })

  output$risk_cards <- renderUI({
    df <- predictions()
    fluidRow(lapply(seq_len(nrow(df)), function(i) {
      cls <- df$Risk[i]
      bg <- switch(cls, Low = "linear-gradient(135deg,#2ca25f,#238b45)", Moderate = "linear-gradient(135deg,#f39c12,#d98200)", High = "linear-gradient(135deg,#de2d26,#a50f15)", "linear-gradient(135deg,#315f8f,#1f3a5f)")
      column(4, div(class = "risk-card", style = paste0("background:", bg, ";"),
        div(class = "name", df$Outcome[i]),
        h3(fmt_prob(df$Probability[i])),
        span(class = "tag", paste(cls, "relative risk")),
        div(style = "font-size:12px;opacity:.88;margin-top:8px;", paste0("AUC ", sprintf("%.3f", df$AUC[i]), " | Features ", df$N_Features[i]))
      ))
    }))
  })

  output$risk_plot <- renderPlot({
    df <- predictions()
    df$Outcome <- factor(df$Outcome, levels = rev(df$Outcome))
    ggplot(df, aes(x = Outcome, y = Probability, fill = Risk)) +
      geom_col(width = 0.72, alpha = 0.95) +
      geom_text(aes(label = fmt_prob(Probability)), hjust = -0.08, size = 4.2, fontface = "bold") +
      coord_flip() +
      scale_fill_manual(values = risk_palette, drop = FALSE) +
      scale_y_continuous(labels = function(x) paste0(round(100*x), "%"), limits = c(0, max(0.05, min(1, max(df$Probability, na.rm = TRUE) * 1.20)))) +
      labs(x = NULL, y = "Predicted probability", fill = "Risk stratum") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank(), legend.position = "bottom", axis.text.y = element_text(face = "bold"))
  })

  output$gauge_plot <- renderPlot({
    df <- predictions()
    ggplot(df, aes(x = Group, y = Probability, color = Risk)) +
      geom_jitter(width = 0.18, height = 0, size = 5, alpha = 0.9) +
      geom_boxplot(width = 0.38, alpha = 0.10, outlier.shape = NA, color = "#7f8c8d") +
      scale_color_manual(values = risk_palette, drop = FALSE) +
      scale_y_continuous(labels = function(x) paste0(round(100*x), "%"), limits = c(0, max(0.05, min(1, max(df$Probability, na.rm = TRUE) * 1.20)))) +
      labs(x = NULL, y = "Predicted probability", color = "Risk") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.text.x = element_text(face = "bold"))
  })

  output$risk_table <- renderDT({
    df <- predictions()
    show <- df
    show$Probability <- fmt_prob(show$Probability)
    show$AUC <- sprintf("%.3f", show$AUC)
    show$Brier <- sprintf("%.3f", show$Brier)
    datatable(show[, c("Outcome", "Group", "Probability", "Risk", "AUC", "Brier", "N_Features")], rownames = FALSE, options = list(pageLength = 11, scrollX = TRUE, dom = "tip"))
  })

  output$risk_interpretation <- renderUI({
    df <- predictions()
    if (nrow(df) == 0) return(div(class = "help-note", "No prediction available."))
    top <- df[1, ]
    tagList(
      div(class = "explain-box",
          strong("Highest predicted endpoint: "), top$Outcome,
          tags$br(), strong("Predicted probability: "), fmt_prob(top$Probability),
          tags$br(), strong("Relative risk stratum: "), top$Risk,
          tags$br(), tags$br(), risk_text(top$Risk)
      ),
      tags$ul(lapply(seq_len(nrow(df)), function(i) {
        tags$li(strong(df$Outcome[i]), paste0(": ", fmt_prob(df$Probability[i]), " (", df$Risk[i], " relative stratum)"))
      }))
    )
  })

  output$download_predictions <- downloadHandler(
    filename = function() paste0("diabetes_vascular_predictions_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- predictions()
      df$ProbabilityPercent <- round(100 * df$Probability, 3)
      write.csv(df, file, row.names = FALSE)
    }
  )


  ai_target_vars <- reactive({
    if (isTRUE(input$ai_selected_only)) required_vars() else sort(unique(unlist(manifest$features_list)))
  })

  observeEvent(input$build_prompt, {
    vars <- ai_target_vars()
    shiny::validate(shiny::need(length(vars) > 0, "Select at least one outcome first."))
    updateTextAreaInput(session, "ai_prompt", value = make_ai_prompt(vars, var_meta))
  })

  ai_extracted <- reactiveVal(NULL)
  ai_raw <- reactiveVal("")
  ai_error <- reactiveVal(NULL)

  observeEvent(input$run_ai_extract, {
    ai_error(NULL)
    ai_extracted(NULL)
    ai_raw("")
    vars <- ai_target_vars()
    shiny::validate(shiny::need(length(vars) > 0, "Select at least one outcome or disable selected-outcome mode."))
    shiny::validate(shiny::need(nzchar(input$ai_api_base), "Please enter API base URL."))
    shiny::validate(shiny::need(nzchar(input$ai_api_key), "Please enter API key."))
    shiny::validate(shiny::need(nzchar(input$ai_model), "Please enter model name."))
    shiny::validate(shiny::need(!is.null(input$ai_file), "Please upload an image or table."))
    prompt <- input$ai_prompt
    if (is.null(prompt) || !nzchar(prompt)) prompt <- make_ai_prompt(vars, var_meta)
    f <- input$ai_file
    ext <- tolower(tools::file_ext(f$name))
    table_text <- NULL
    img_path <- NULL
    if (ext %in% c("csv", "tsv", "txt", "xls", "xlsx")) {
      table_text <- read_uploaded_table_text(f$datapath, f$name)
    } else {
      img_path <- f$datapath
    }
    tryCatch({
      res_txt <- call_openai_compatible_vision(
        api_base = input$ai_api_base,
        api_key = input$ai_api_key,
        model = input$ai_model,
        prompt = prompt,
        file_path = img_path,
        file_name = f$name,
        table_text = table_text
      )
      ai_raw(res_txt)
      js <- jsonlite::fromJSON(extract_json_text(res_txt), simplifyVector = FALSE)
      vals <- js$values
      if (is.null(vals)) vals <- js
      out <- data.frame(
        variable = vars,
        label = label_for(vars),
        extracted_value = NA_real_,
        evidence = NA_character_,
        status = "missing",
        stringsAsFactors = FALSE
      )
      for (i in seq_along(vars)) {
        v <- vars[i]
        val <- vals[[v]]
        if (!is.null(val) && length(val) > 0 && !is.na(val)) {
          num <- suppressWarnings(as.numeric(val))
          if (is.finite(num)) {
            out$extracted_value[i] <- num
            out$status[i] <- "extracted"
          }
        }
        ev <- tryCatch(js$evidence[[v]], error = function(e) NULL)
        if (!is.null(ev) && length(ev) > 0) out$evidence[i] <- as.character(ev)[1]
      }
      ai_extracted(out)
    }, error = function(e) {
      ai_error(conditionMessage(e))
      ai_raw(conditionMessage(e))
    })
  })

  output$ai_status <- renderUI({
    err <- ai_error()
    dat <- ai_extracted()
    if (!is.null(err)) return(div(class = "map-miss", paste("AI extraction failed:", err)))
    if (is.null(dat)) return(div(class = "help-note", "No AI extraction has been run yet."))
    div(
      span(class = "stat-pill", paste(sum(dat$status == "extracted"), "value(s) extracted")),
      span(class = "stat-pill", paste(sum(dat$status != "extracted"), "missing/uncertain"))
    )
  })

  output$ai_raw_response <- renderText({
    txt <- ai_raw()
    if (!nzchar(txt)) "AI JSON response will appear here."
    else txt
  })

  output$ai_mapping_table <- renderDT({
    dat <- ai_extracted()
    if (is.null(dat)) {
      dat <- data.frame(variable = ai_target_vars(), label = label_for(ai_target_vars()), extracted_value = NA_real_, evidence = NA_character_, status = "not run")
    }
    datatable(dat, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  observeEvent(input$apply_ai_values, {
    dat <- ai_extracted()
    shiny::validate(shiny::need(!is.null(dat), "Run AI extraction first."))
    ok <- dat[dat$status == "extracted" & is.finite(dat$extracted_value), ]
    for (i in seq_len(nrow(ok))) {
      id <- var_meta$input_id[match(ok$variable[i], var_meta$variable)]
      if (!is.na(id)) updateNumericInput(session, id, value = ok$extracted_value[i])
    }
    showNotification(paste("Applied", nrow(ok), "extracted values to the form."), type = "message")
  })

  output$download_template <- downloadHandler(
    filename = function() paste0("diabetes_vascular_batch_template_", format(Sys.time(), "%Y%m%d"), ".csv"),
    content = function(file) {
      vars <- sort(unique(unlist(manifest$features_list)))
      one <- as.data.frame(as.list(setNames(var_meta$default[match(vars, var_meta$variable)], vars)), check.names = FALSE)
      one <- cbind(ID = "Example_001", one)
      write.csv(one, file, row.names = FALSE)
    }
  )

  batch_results <- eventReactive(input$run_batch, {
    shiny::validate(shiny::need(!is.null(input$batch_file), "Please upload a CSV file."))
    dat <- read.csv(input$batch_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    shiny::validate(shiny::need(nrow(dat) > 0, "Uploaded CSV has no rows."))
    outs <- if (isTRUE(input$batch_selected_only)) selected_outcomes() else manifest$outcome_id
    shiny::validate(shiny::need(length(outs) > 0, "Select at least one outcome or disable selected-outcome mode."))
    req_vars <- unique(unlist(manifest$features_list[match(outs, manifest$outcome_id)]))
    missing <- setdiff(req_vars, names(dat))
    shiny::validate(shiny::need(length(missing) == 0, paste("Missing required columns:", paste(missing, collapse = ", "))))
    ids <- if ("ID" %in% names(dat)) as.character(dat$ID) else paste0("Patient_", seq_len(nrow(dat)))
    out_rows <- list()
    k <- 1
    for (i in seq_len(nrow(dat))) {
      vals <- setNames(rep(NA_real_, length(req_vars)), req_vars)
      for (v in req_vars) vals[[v]] <- suppressWarnings(as.numeric(dat[[v]][i]))
      if (!all(is.finite(vals))) next
      pred <- predict_values_for_outcomes(vals, outs)
      pred$ID <- ids[i]
      pred$Row <- i
      out_rows[[k]] <- pred
      k <- k + 1
    }
    shiny::validate(shiny::need(length(out_rows) > 0, "No valid complete rows were available for prediction."))
    ans <- do.call(rbind, out_rows)
    ans <- ans[, c("ID", "Row", "outcome_id", "Outcome", "Group", "Probability", "ProbabilityPercent", "Risk", "AUC", "Brier", "N_Features")]
    rownames(ans) <- NULL
    ans
  })

  output$batch_summary <- renderUI({
    df <- batch_results()
    div(
      span(class = "stat-pill", paste(length(unique(df$ID)), "patient(s) scored")),
      span(class = "stat-pill", paste(length(unique(df$outcome_id)), "outcome(s)")),
      span(class = "stat-pill", paste(sum(df$Risk == "High"), "high-stratum predictions"))
    )
  })

  output$batch_plot <- renderPlot({
    df <- batch_results()
    ggplot(df, aes(x = Outcome, fill = Risk)) +
      geom_bar(position = "stack", alpha = 0.92) +
      coord_flip() +
      scale_fill_manual(values = risk_palette, drop = FALSE) +
      labs(x = NULL, y = "Number of patient-outcome predictions", fill = "Risk") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), legend.position = "bottom", axis.text.y = element_text(face = "bold"))
  })

  output$batch_table <- renderDT({
    df <- batch_results()
    show <- df
    show$Probability <- fmt_prob(show$Probability)
    show$AUC <- sprintf("%.3f", show$AUC)
    show$Brier <- sprintf("%.3f", show$Brier)
    datatable(show, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$download_batch <- downloadHandler(
    filename = function() paste0("diabetes_vascular_batch_predictions_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) write.csv(batch_results(), file, row.names = FALSE)
  )

  output$var_chips <- renderUI({
    vars <- required_vars()
    if (length(vars) == 0) return(div(class = "help-note", "No variables selected."))
    tags$div(lapply(vars, function(v) span(class = "var-chip", paste0(label_for(v), ifelse(nchar(unit_for(v)) > 0, paste0(" [", unit_for(v), "]"), "")))))
  })

  observe({
    outs <- selected_outcomes()
    choices <- if (length(outs) > 0) setNames(outs, manifest$outcome_label[match(outs, manifest$outcome_id)]) else setNames(manifest$outcome_id, manifest$outcome_label)
    updateSelectInput(session, "explain_outcome", choices = choices, selected = choices[1])
  })

  output$deviation_plot <- renderPlot({
    df <- head(explain_selected(), 15)
    df$Variable <- factor(df$Variable, levels = rev(df$Variable))
    ggplot(df, aes(x = Variable, y = AbsStandardizedValue)) +
      geom_col(fill = "#315f8f", width = 0.72, alpha = 0.88) +
      geom_text(aes(label = sprintf("%.2f", StandardizedValue)), hjust = -0.10, size = 3.8, fontface = "bold") +
      coord_flip() +
      labs(x = NULL, y = "Absolute standardized deviation", caption = "Labels show signed standardized values.") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold")) +
      expand_limits(y = max(df$AbsStandardizedValue, na.rm = TRUE) * 1.18)
  })

  output$deviation_table <- renderDT({
    df <- explain_selected()
    df$RawValue <- round(df$RawValue, 3)
    df$StandardizedValue <- round(df$StandardizedValue, 3)
    df$AbsStandardizedValue <- round(df$AbsStandardizedValue, 3)
    datatable(df[, c("Variable", "Feature", "RawValue", "Unit", "StandardizedValue", "AbsStandardizedValue")], rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$model_table <- renderDT({
    df <- manifest[, c("outcome_label", "group", "n_features", "auc", "brier", "accuracy", "f1", "recall")]
    names(df) <- c("Outcome", "Group", "N Features", "Mean AUC", "Mean Brier", "Accuracy", "F1", "Recall")
    num_cols <- c("Mean AUC", "Mean Brier", "Accuracy", "F1", "Recall")
    for (cc in num_cols) df[[cc]] <- sprintf("%.3f", as.numeric(df[[cc]]))
    datatable(df, rownames = FALSE, options = list(pageLength = 11, scrollX = TRUE, dom = "tip"))
  })
}

shinyApp(ui = ui, server = server)


