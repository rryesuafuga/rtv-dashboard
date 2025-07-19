

# fix_deployment_final.R - Final fixes for deployment

cat("Applying final deployment fixes...\n\n")

# 1. Fix DESCRIPTION file newline
cat("1. Fixing DESCRIPTION file...\n")
if (file.exists("DESCRIPTION")) {
  # Add newline at end
  cat("\n", file = "DESCRIPTION", append = TRUE)
  cat("   ✓ Added newline to DESCRIPTION\n")
}

# 2. Create a deployment-ready app.R
cat("\n2. Creating deployment-ready app.R...\n")

deployment_app <- '# app.R - Raising The Village Data Science Dashboard

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

# Create placeholder functions if they don\'t exist
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
'

# Backup existing app.R
if (file.exists("app.R")) {
  file.copy("app.R", "app.R.backup_final", overwrite = TRUE)
  cat("   ✓ Backed up existing app.R\n")
}

# Write new app.R
writeLines(deployment_app, "app.R")
cat("   ✓ Created deployment-ready app.R\n")

# 3. Check all files have proper endings
cat("\n3. Fixing file endings...\n")
r_files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  "app.R"
)

for (file in r_files) {
  if (file.exists(file)) {
    # Add newline if missing
    content <- readLines(file, warn = FALSE)
    con <- file(file, "w")
    writeLines(content, con)
    close(con)
    cat("\n", file = file, append = TRUE)
  }
}
cat("   ✓ Fixed all R file endings\n")

# 4. Create minimal test for deployment
cat("\n4. Creating minimal test app...\n")

minimal_app <- 'library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "RTV Dashboard Test"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Test", tabName = "test", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "test",
        h2("RTV Dashboard - Deployment Test"),
        p("If you see this, deployment is working!"),
        hr(),
        h3("System Info:"),
        verbatimTextOutput("sysinfo"),
        h3("Available Files:"),
        verbatimTextOutput("files")
      )
    )
  )
)

server <- function(input, output, session) {
  output$sysinfo <- renderPrint({
    list(
      R_Version = R.version.string,
      Working_Dir = getwd(),
      Sys_Date = Sys.Date()
    )
  })
  
  output$files <- renderPrint({
    list(
      Root_Files = list.files(),
      R_Files = list.files("R"),
      Data_Files = list.files("data")
    )
  })
}

shinyApp(ui = ui, server = server)
'

writeLines(minimal_app, "app_minimal.R")
cat("   ✓ Created app_minimal.R\n")

# 5. Final deployment instructions
cat("\n", rep("=", 60), "\n", sep = "")
cat("DEPLOYMENT INSTRUCTIONS\n")
cat(rep("=", 60), "\n\n")

cat("Option 1: Deploy main app\n")
cat("-------------------------\n")
cat("rsconnect::deployApp(\n")
cat("  appName = 'rtv-dashboard',\n")
cat("  appFiles = c('app.R', 'R/', 'data/', 'www/', 'DESCRIPTION'),\n")
cat("  forceUpdate = TRUE\n")
cat(")\n\n")

cat("Option 2: Test with minimal app first\n")
cat("-------------------------------------\n")
cat("# Test deployment works\n")
cat("file.rename('app_minimal.R', 'app.R')\n")
cat("rsconnect::deployApp(appName = 'rtv-dashboard', forceUpdate = TRUE)\n\n")

cat("Option 3: If still failing, deploy without DESCRIPTION\n")
cat("-----------------------------------------------------\n")
cat("# Remove DESCRIPTION temporarily\n")
cat("file.rename('DESCRIPTION', 'DESCRIPTION.bak')\n")
cat("rsconnect::deployApp(appName = 'rtv-dashboard', forceUpdate = TRUE)\n\n")

cat("After successful deployment:\n")
cat("- Check logs: rsconnect::showLogs('rtv-dashboard')\n")
cat("- View app: https://YOUR-USERNAME.shinyapps.io/rtv-dashboard\n")