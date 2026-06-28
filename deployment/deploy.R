library(rsconnect)

# Deployment note:
# 1. Configure your rsconnect account once in the R console or local .Renviron.
# 2. Do not hard-code token/secret in this script.
# 3. Run this file from the Shiny_DiaComplication_11Models directory.

rsconnect::deployApp(
  appDir = ".",
  appName = "DiaComplication-11Models-Calculator",
  forceUpdate = TRUE
)
