
# deploy.R - Deployment script for RTV Dashboard

# Load required libraries
library(rsconnect)
library(devtools)

# Function to prepare and deploy the app
deploy_rtv_dashboard <- function(
    app_name = "rtv-data-dashboard",
    account_name = NULL,
    force_update = FALSE
) {
  
  message("Starting deployment process for RTV Dashboard...")
  
  # Check if account is configured
  if (is.null(account_name)) {
    accounts <- rsconnect::accounts()
    if (nrow(accounts) == 0) {
      stop("No shinyapps.io account configured. Please run rsconnect::setAccountInfo()")
    }
    account_name <- accounts$name[1]
  }
  
  # Check required files exist
  required_files <- c(
    "app.R",
    "R/app_ui.R",
    "R/app_server.R",
    "R/mod_overview.R",
    "R/mod_vulnerability_scoring.R"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop("Missing required files: ", paste(missing_files, collapse = ", "))
  }
  
  # Check data files
  message("Checking data files...")
  data_files <- c(
    "data/household_vulnerability_data.csv",
    "data/climate_risk_data.csv",
    "data/crop_diagnostics_data.csv",
    "data/program_impact_metrics.csv",
    "data/field_officer_activity.csv",
    "agricultureandruraldevelopment_uga.csv"
  )
  
  missing_data <- data_files[!file.exists(data_files)]
  if (length(missing_data) > 0) {
    message("Missing data files. Generating pseudo data...")
    source("generate_pseudo_data.R")
  }
  
  # Create app manifest
  message("Creating app manifest...")
  app_files <- c(
    "app.R",
    list.files("R", full.names = TRUE),
    list.files("data", full.names = TRUE),
    list.files("www", full.names = TRUE),
    "agricultureandruraldevelopment_uga.csv"
  )
  
  # Check total size
  total_size <- sum(file.info(app_files)$size) / 1024^2
  message(sprintf("Total app size: %.2f MB", total_size))
  
  if (total_size > 100) {
    warning("App size exceeds 100MB. Consider reducing data size.")
  }
  
  # Deploy to shinyapps.io
  message("Deploying to shinyapps.io...")
  
  rsconnect::deployApp(
    appDir = ".",
    appName = app_name,
    appTitle = "RTV Data Science Dashboard",
    account = account_name,
    forceUpdate = force_update,
    lint = TRUE,
    appFiles = app_files
  )
  
  # Get app URL
  app_url <- paste0("https://", account_name, ".shinyapps.io/", app_name)
  message("\nDeployment complete!")
  message("App URL: ", app_url)
  
  # Return URL
  invisible(app_url)
}

# Function to configure shinyapps.io account
configure_shinyapps_account <- function() {
  cat("To deploy to shinyapps.io, you need to configure your account.\n")
  cat("1. Go to https://www.shinyapps.io/\n")
  cat("2. Sign up or log in\n")
  cat("3. Go to Account -> Tokens\n")
  cat("4. Click 'Show' next to your token\n")
  cat("5. Copy the rsconnect::setAccountInfo() command\n")
  cat("6. Paste and run it here\n\n")
}

# Function to test deployment locally
test_deployment <- function(port = 3838) {
  message("Testing deployment locally...")
  
  # Check all modules load
  tryCatch({
    source("R/app_ui.R")
    source("R/app_server.R")
    source("R/mod_overview.R")
    source("R/mod_vulnerability_scoring.R")
    message("✓ All modules loaded successfully")
  }, error = function(e) {
    stop("Error loading modules: ", e$message)
  })
  
  # Run app locally
  message("Starting app on port ", port, "...")
  shiny::runApp(port = port, launch.browser = TRUE)
}

# Main deployment workflow
if (interactive()) {
  cat("RTV Dashboard Deployment Script\n")
  cat("==============================\n\n")
  cat("Options:\n")
  cat("1. Deploy to shinyapps.io\n")
  cat("2. Test locally\n")
  cat("3. Configure shinyapps.io account\n")
  cat("4. Exit\n\n")
  
  choice <- readline("Enter your choice (1-4): ")
  
  switch(choice,
         "1" = {
           account <- readline("Enter shinyapps.io account name (leave blank for default): ")
           if (account == "") account <- NULL
           deploy_rtv_dashboard(account_name = account)
         },
         "2" = test_deployment(),
         "3" = configure_shinyapps_account(),
         "4" = message("Exiting..."),
         message("Invalid choice")
  )
}
