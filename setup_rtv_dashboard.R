

# setup_rtv_dashboard.R - Automated setup script for RTV Dashboard

cat("====================================\n")
cat("RTV Dashboard Setup Script\n")
cat("====================================\n\n")

# Function to create directory if it doesn't exist
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cat("✓ Created directory:", path, "\n")
  } else {
    cat("• Directory exists:", path, "\n")
  }
}

# Function to write content to file
write_file <- function(content, filepath) {
  if (!file.exists(filepath)) {
    writeLines(content, filepath)
    cat("✓ Created file:", filepath, "\n")
  } else {
    cat("• File exists:", filepath, "\n")
  }
}

# Create directory structure
cat("\n1. Creating directory structure...\n")
create_dir("R")
create_dir("data")
create_dir("www")
create_dir("tests/testthat")
create_dir("inst/app/www")

# Create a simple logo placeholder
cat("\n2. Creating placeholder logo...\n")
if (!file.exists("www/rtv_logo.png")) {
  # Create a simple SVG logo and save as PNG placeholder
  svg_content <- '
<svg width="200" height="50" xmlns="http://www.w3.org/2000/svg">
  <rect width="200" height="50" fill="#2E7D32" rx="5"/>
  <text x="100" y="30" font-family="Arial, sans-serif" font-size="20" font-weight="bold" 
        text-anchor="middle" fill="white">RTV Dashboard</text>
</svg>'
  
  write_file(svg_content, "www/rtv_logo.svg")
  cat("✓ Created placeholder logo (SVG)\n")
  cat("  Note: Convert to PNG or replace with actual logo\n")
}

# Create golem config
cat("\n3. Creating golem configuration...\n")
golem_config <- 'default:
  golem_name: rtvdashboard
  golem_version: 0.0.0.9000
  app_prod: no
  app_title: RTV Data Science Dashboard
production:
  app_prod: yes
dev:
  golem_wd: !expr here::here()
'
create_dir("inst")
write_file(golem_config, "inst/golem-config.yml")

# Create .Rbuildignore
cat("\n4. Creating .Rbuildignore...\n")
rbuildignore_content <- '^.*\\.Rproj$
^\\.Rproj\\.user$
^data$
^dev$
^README\\.md$
^LICENSE\\.md$
^\\.github$
^app\\.R$
^rsconnect$
^deploy\\.R$
^setup_rtv_dashboard\\.R$
^generate_pseudo_data\\.R$
'
write_file(rbuildignore_content, ".Rbuildignore")

# Create .gitignore
cat("\n5. Creating .gitignore...\n")
gitignore_content <- '.Rproj.user
.Rhistory
.RData
.Ruserdata
.DS_Store
*.Rproj
rsconnect/
cache/
*.log
.httr-oauth
.secrets
'
write_file(gitignore_content, ".gitignore")

# Check for World Bank data
cat("\n6. Checking for World Bank data...\n")
if (!file.exists("agricultureandruraldevelopment_uga.csv")) {
  cat("⚠ Warning: World Bank data file not found\n")
  cat("  Please ensure 'agricultureandruraldevelopment_uga.csv' is in the root directory\n")
  
  # Create a minimal placeholder
  placeholder_data <- data.frame(
    `Country Name` = "Uganda",
    `Country ISO3` = "UGA",
    Year = 2020:2023,
    `Indicator Name` = "Agricultural land (% of land area)",
    `Indicator Code` = "AG.LND.AGRI.ZS",
    Value = runif(4, 70, 75),
    check.names = FALSE
  )
  write.csv(placeholder_data, "agricultureandruraldevelopment_uga.csv", row.names = FALSE)
  cat("✓ Created placeholder World Bank data\n")
}

# Check if all required packages are installed
cat("\n7. Checking required packages...\n")
required_packages <- c(
  "shiny", "golem", "shinydashboard", "shinydashboardPlus",
  "tidyverse", "plotly", "leaflet", "DT", "shinyWidgets",
  "config", "testthat", "rsconnect", "lubridate", "readxl"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  cat("\n⚠ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("\nInstall missing packages? (y/n): ")
  answer <- tolower(readline())
  
  if (answer == "y") {
    install.packages(missing_packages)
    cat("✓ Packages installed\n")
  }
} else {
  cat("✓ All required packages are installed\n")
}

# Generate data if not exists
cat("\n8. Checking for data files...\n")
data_files <- c(
  "data/household_vulnerability_data.csv",
  "data/climate_risk_data.csv",
  "data/crop_diagnostics_data.csv",
  "data/program_impact_metrics.csv",
  "data/field_officer_activity.csv"
)

if (!all(file.exists(data_files))) {
  cat("⚠ Some data files are missing\n")
  cat("\nGenerate pseudo data? (y/n): ")
  answer <- tolower(readline())
  
  if (answer == "y") {
    if (file.exists("generate_pseudo_data.R")) {
      source("generate_pseudo_data.R")
      cat("✓ Data generated\n")
    } else {
      cat("⚠ generate_pseudo_data.R not found\n")
      cat("  Please run the data generation script manually\n")
    }
  }
} else {
  cat("✓ All data files present\n")
}

# Create a run script
cat("\n9. Creating run script...\n")
run_script <- '#!/usr/bin/env Rscript
# run_app.R - Quick launch script

library(shiny)
runApp(launch.browser = TRUE, port = 3838)
'
write_file(run_script, "run_app.R")

# Final summary
cat("\n====================================\n")
cat("Setup Complete!\n")
cat("====================================\n\n")

cat("Next steps:\n")
cat("1. Ensure all R/*.R files are in place\n")
cat("2. Run: source('generate_pseudo_data.R') if data is missing\n")
cat("3. Test locally: shiny::runApp()\n")
cat("4. Deploy: source('deploy.R')\n")

cat("\nDirectory structure:\n")
cat("rtv-dashboard/\n")
cat("├── R/              # All module files\n")
cat("├── data/           # Generated datasets\n")
cat("├── www/            # CSS, JS, and assets\n")
cat("├── tests/          # Unit tests\n")
cat("├── inst/           # Package files\n")
cat("├── app.R           # Main app file\n")
cat("└── DESCRIPTION     # Package description\n")

cat("\n✓ Setup script completed successfully!\n")
