# app.R - Raising The Village Data Science Dashboard

# Load all required libraries
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

# Function to safely source files
safe_source <- function(file) {
  if (file.exists(file)) {
    source(file)
  } else {
    warning(paste("File not found:", file))
  }
}

# Source all required files
safe_source("R/app_config.R")
safe_source("R/app_server.R")
safe_source("R/app_ui.R")
safe_source("R/fct_helpers.R")
safe_source("R/utils_helpers.R")
safe_source("R/mod_overview.R")
safe_source("R/mod_vulnerability_scoring.R")
safe_source("R/mod_climate_risk.R")
safe_source("R/mod_crop_diagnostics.R")
safe_source("R/mod_impact_metrics.R")
safe_source("R/mod_field_activities.R")

# Create placeholder functions if they don't exist
if (!exists("app_ui")) {
  app_ui <- function(request) {
    dashboardPage(
      dashboardHeader(title = "RTV Dashboard"),
      dashboardSidebar(),
      dashboardBody(
        h2("Loading...")
      )
    )
  }
}

if (!exists("app_server")) {
  app_server <- function(input, output, session) {
    # Empty server
  }
}

# Create and return the Shiny app
shinyApp(ui = app_ui, server = app_server)


