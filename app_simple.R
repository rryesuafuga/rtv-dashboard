# app.R - Raising The Village Data Science Dashboard
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

