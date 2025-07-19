

# create_missing_files.R - Script to create any missing files

# Create R/utils_helpers.R if it doesn't exist
if (!file.exists("R/utils_helpers.R")) {
  cat("Creating R/utils_helpers.R...\n")
  
  utils_content <- '# R/utils_helpers.R - Utility functions

#\' Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#\' Safe division helper
#\' @export
safe_div <- function(x, y, default = 0) {
  ifelse(y == 0, default, x / y)
}
'

writeLines(utils_content, "R/utils_helpers.R")
cat("✓ Created R/utils_helpers.R\n")
}

# Create the missing module files from the artifacts
if (!file.exists("R/mod_impact_metrics.R")) {
  cat("Creating R/mod_impact_metrics.R...\n")
  
  impact_content <- '# R/mod_impact_metrics.R - Impact Metrics Module

#\' impact_metrics UI Function
#\'
#\' @description Program impact and ROI metrics module
#\' @param id,input,output,session Internal parameters for {shiny}.
#\' @noRd 
#\' @importFrom shiny NS tagList 
mod_impact_metrics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Program Impact Metrics",
        status = "primary",
        solidHeader = TRUE,
        
        tabsetPanel(
          tabPanel("ROI Analysis",
            br(),
            plotlyOutput(ns("roi_timeline"), height = 400)
          ),
          tabPanel("Beneficiary Growth",
            br(),
            plotlyOutput(ns("beneficiary_growth"), height = 400)
          ),
          tabPanel("Income Impact",
            br(),
            plotlyOutput(ns("income_impact"), height = 400)
          )
        )
      )
    )
  )
}

#\' impact_metrics Server Functions
#\' @noRd 
mod_impact_metrics_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ROI Timeline
    output$roi_timeline <- renderPlotly({
      months <- seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month")
      roi_data <- data.frame(
        date = months,
        roi = cumsum(runif(length(months), 5, 15))
      )
      
      plot_ly(roi_data, x = ~date, y = ~roi,
              type = "scatter", mode = "lines",
              fill = "tozeroy",
              line = list(color = "#4CAF50")) %>%
        layout(
          title = "Cumulative ROI Over Time",
          xaxis = list(title = "Date"),
          yaxis = list(title = "ROI (%)")
        )
    })
    
    # Beneficiary Growth
    output$beneficiary_growth <- renderPlotly({
      growth_data <- data.frame(
        year = 2020:2024,
        beneficiaries = c(5000, 12000, 25000, 48000, 85000)
      )
      
      plot_ly(growth_data, x = ~year, y = ~beneficiaries,
              type = "bar",
              marker = list(color = "#2196F3")) %>%
        layout(
          title = "Beneficiary Growth by Year",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Total Beneficiaries")
        )
    })
    
    # Income Impact
    output$income_impact <- renderPlotly({
      phase_data <- data.frame(
        phase = c("Baseline", "6 Months", "12 Months", "18 Months", "24 Months"),
        income = c(0.85, 1.20, 1.65, 2.10, 2.27)
      )
      
      plot_ly(phase_data, x = ~phase, y = ~income,
              type = "scatter", mode = "lines+markers",
              line = list(color = "#FF9800", width = 3),
              marker = list(size = 10)) %>%
        layout(
          title = "Average Daily Income Progression (USD)",
          xaxis = list(title = "Program Phase"),
          yaxis = list(title = "Daily Income (USD)")
        )
    })
  })
}
'

writeLines(impact_content, "R/mod_impact_metrics.R")
cat("✓ Created R/mod_impact_metrics.R\n")
}

if (!file.exists("R/mod_field_activities.R")) {
  cat("Creating R/mod_field_activities.R...\n")
  
  field_content <- '# R/mod_field_activities.R - Field Activities Module

#\' field_activities UI Function
#\'
#\' @description Field officer activities tracking module
#\' @param id,input,output,session Internal parameters for {shiny}.
#\' @noRd 
#\' @importFrom shiny NS tagList 
mod_field_activities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 8,
        title = "Field Activity Map",
        status = "info",
        solidHeader = TRUE,
        leafletOutput(ns("activity_map"), height = 500)
      ),
      
      box(
        width = 4,
        title = "Activity Summary",
        status = "success",
        solidHeader = TRUE,
        
        infoBoxOutput(ns("total_activities"), width = 12),
        br(),
        infoBoxOutput(ns("active_officers"), width = 12),
        br(),
        infoBoxOutput(ns("villages_visited"), width = 12)
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Field Officer Performance",
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput(ns("officer_table"))
      )
    )
  )
}

#\' field_activities Server Functions
#\' @noRd 
mod_field_activities_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Activity map
    output$activity_map <- renderLeaflet({
      # Generate sample locations
      set.seed(123)
      activities <- data.frame(
        lat = runif(50, -0.6, 0.0),
        lon = runif(50, 30.5, 31.0),
        activity = sample(c("Training", "Assessment", "VSLA Meeting"), 50, replace = TRUE)
      )
      
      leaflet(activities) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius = 6,
          color = ~case_when(
            activity == "Training" ~ "#4CAF50",
            activity == "Assessment" ~ "#2196F3",
            TRUE ~ "#FF9800"
          ),
          fillOpacity = 0.7,
          popup = ~activity
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("#4CAF50", "#2196F3", "#FF9800"),
          labels = c("Training", "Assessment", "VSLA Meeting"),
          title = "Activity Type"
        )
    })
    
    # Info boxes
    output$total_activities <- renderInfoBox({
      infoBox(
        "Total Activities",
        "15,234",
        icon = icon("calendar-check"),
        color = "blue"
      )
    })
    
    output$active_officers <- renderInfoBox({
      infoBox(
        "Active Officers",
        "20",
        icon = icon("users"),
        color = "green"
      )
    })
    
    output$villages_visited <- renderInfoBox({
      infoBox(
        "Villages Visited",
        "156",
        icon = icon("map-marker-alt"),
        color = "yellow"
      )
    })
    
    # Officer performance table
    output$officer_table <- DT::renderDataTable({
      officer_data <- data.frame(
        Officer = paste("Officer", LETTERS[1:10]),
        `Activities This Month` = sample(50:150, 10),
        `Households Reached` = sample(200:500, 10),
        `Avg Duration (hrs)` = round(runif(10, 1.5, 3.5), 1),
        Performance = sample(c("Excellent", "Good", "Average"), 10, 
                           prob = c(0.4, 0.4, 0.2), replace = TRUE),
        check.names = FALSE
      )
      
      DT::datatable(
        officer_data,
        options = list(
          pageLength = 5,
          dom = "frtip"
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Performance",
          backgroundColor = DT::styleEqual(
            c("Excellent", "Good", "Average"),
            c("#C8E6C9", "#FFF9C4", "#FFCCBC")
          )
        )
    })
  })
}
'

writeLines(field_content, "R/mod_field_activities.R")
cat("✓ Created R/mod_field_activities.R\n")
}

# Also update the app_ui.R file to fix the resource path issue
if (file.exists("R/app_ui.R")) {
  cat("\nUpdating R/app_ui.R to fix resource path issue...\n")
  
  # Read the current file
  current_content <- readLines("R/app_ui.R")
  
  # Find and replace the golem_add_external_resources function
  start_line <- grep("golem_add_external_resources <- function", current_content)
  if (length(start_line) > 0) {
    # Find the end of the function (next function or end of file)
    end_line <- length(current_content)
    next_func <- grep("^[a-zA-Z_].*<- function", current_content[(start_line[1]+1):length(current_content)])
    if (length(next_func) > 0) {
      end_line <- start_line[1] + next_func[1] - 1
    }
    
    # Replace with fixed version
    new_function <- 'golem_add_external_resources <- function() {
  
  # Check if www directory exists
  if (dir.exists("www")) {
    addResourcePath("www", "www")
  }
  
  tags$head(
    # Add custom CSS and JS
    if (file.exists("www/custom.css")) {
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    },
    if (file.exists("www/custom.js")) {
      tags$script(src = "www/custom.js")
    },
    
    # Add custom meta tags
    tags$meta(name = "description", content = "Raising The Village Data Science Dashboard"),
    tags$meta(name = "author", content = "RTV Data Team"),
    
    # Add favicon if exists
    if (file.exists("www/favicon.ico")) {
      tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico")
    }
  )
}'
    
    # Rebuild the content
    new_content <- c(
      current_content[1:(start_line[1]-1)],
      new_function,
      if (end_line < length(current_content)) current_content[(end_line+1):length(current_content)] else character(0)
    )
    
    # Write back
    writeLines(new_content, "R/app_ui.R")
    cat("✓ Updated R/app_ui.R\n")
  }
}

cat("\n✓ All missing files created!\n")
cat("\nNow run: shiny::runApp()\n")
