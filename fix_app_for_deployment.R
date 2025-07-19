


# fix_app_for_deployment.R - Fix app.R to work on shinyapps.io

cat("Fixing app.R for shinyapps.io deployment...\n\n")

# Read current app.R
if (file.exists("app.R")) {
  app_content <- readLines("app.R", warn = FALSE)
  
  # Backup current version
  writeLines(app_content, "app.R.backup")
  cat("✓ Backed up current app.R to app.R.backup\n")
  
  # Find the last lines that need to be changed
  last_lines <- tail(app_content, 5)
  
  # Find where the interactive check is
  interactive_line <- grep("if \\(interactive\\(\\)\\)", app_content)
  
  if (length(interactive_line) > 0) {
    # Remove the interactive check and just call run_app()
    new_content <- app_content[1:(interactive_line[length(interactive_line)]-1)]
    new_content <- c(new_content, "", "# Run the app", "run_app()")
  } else {
    # Just add run_app() at the end
    new_content <- c(app_content, "", "# Run the app", "run_app()")
  }
  
  # Write the fixed content
  writeLines(new_content, "app.R")
  cat("✓ Fixed app.R to return shiny app object\n")
  
} else {
  cat("❌ app.R not found!\n")
}

# Also create a simpler alternative if needed
cat("\nCreating alternative simple app.R...\n")

simple_app <- '# app.R - Raising The Village Data Science Dashboard
# Simplified version for shinyapps.io deployment

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)

# Source all modules and functions
source_safely <- function(file) {
  if (file.exists(file)) {
    source(file)
  } else {
    warning(paste("File not found:", file))
  }
}

# Source files
source_safely("R/app_config.R")
source_safely("R/app_server.R")
source_safely("R/app_ui.R")
source_safely("R/fct_helpers.R")
source_safely("R/mod_overview.R")
source_safely("R/mod_vulnerability_scoring.R")
source_safely("R/mod_climate_risk.R")
source_safely("R/mod_crop_diagnostics.R")
source_safely("R/mod_impact_metrics.R")
source_safely("R/mod_field_activities.R")

# Create and run the Shiny application
shinyApp(
  ui = app_ui,
  server = app_server
)
'

writeLines(simple_app, "app_simple.R")
cat("✓ Created app_simple.R as alternative\n")

cat("\n" , rep("=", 50), "\n", sep = "")
cat("DEPLOYMENT INSTRUCTIONS:\n")
cat(rep("=", 50), "\n\n")

cat("Option 1: Deploy with fixed app.R\n")
cat("---------------------------------\n")
cat("rsconnect::deployApp(\n")
cat("  appName = 'rtv-dashboard',\n")
cat("  forceUpdate = TRUE\n")
cat(")\n\n")

cat("Option 2: Deploy with simple app.R\n")
cat("----------------------------------\n")
cat("1. Rename app_simple.R to app.R:\n")
cat("   file.rename('app_simple.R', 'app.R')\n")
cat("2. Deploy:\n")
cat("   rsconnect::deployApp(\n")
cat("     appName = 'rtv-dashboard',\n")
cat("     forceUpdate = TRUE\n")
cat("   )\n\n")

cat("If deployment still fails, check:\n")
cat("- All R/ files exist and are readable\n")
cat("- All data/ files exist\n")
cat("- No syntax errors in any R files\n")