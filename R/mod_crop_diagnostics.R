

# R/mod_crop_diagnostics.R - Crop Diagnostics Module

#' crop_diagnostics UI Function
#'
#' @description Crop diagnostics and yield prediction module
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
mod_crop_diagnostics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("total_assessments")),
      valueBoxOutput(ns("crops_monitored")),
      valueBoxOutput(ns("avg_yield")),
      valueBoxOutput(ns("pest_prevalence"))
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Crop Diagnostics Dashboard",
        status = "success",
        solidHeader = TRUE,
        p("Advanced crop monitoring and diagnostic features coming soon."),
        br(),
        
        # Placeholder for demo
        plotlyOutput(ns("crop_health_demo"), height = 400)
      )
    )
  )
}

#' crop_diagnostics Server Functions
#' @noRd 
mod_crop_diagnostics_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$total_assessments <- renderValueBox({
      valueBox(
        value = "10,245",
        subtitle = "Total Assessments",
        icon = icon("microscope"),
        color = "green"
      )
    })
    
    output$crops_monitored <- renderValueBox({
      valueBox(
        value = "4",
        subtitle = "Crop Types",
        icon = icon("seedling"),
        color = "yellow"
      )
    })
    
    output$avg_yield <- renderValueBox({
      valueBox(
        value = "+23%",
        subtitle = "Yield Improvement",
        icon = icon("chart-line"),
        color = "aqua"
      )
    })
    
    output$pest_prevalence <- renderValueBox({
      valueBox(
        value = "28%",
        subtitle = "Pest Prevalence",
        icon = icon("bug"),
        color = "red"
      )
    })
    
    # Demo visualization
    output$crop_health_demo <- renderPlotly({
      demo_data <- data.frame(
        crop = rep(c("Banana", "Coffee", "Maize", "Beans"), each = 12),
        month = rep(month.abb, 4),
        health_score = runif(48, 60, 95)
      )
      
      plot_ly(demo_data, x = ~month, y = ~health_score, 
              color = ~crop, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = "Crop Health Score by Month",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Health Score (0-100)"),
          hovermode = 'x unified'
        )
    })
  })
}

# R/mod_impact_metrics.R - Impact Metrics Module

#' impact_metrics UI Function
#'
#' @description Program impact and ROI metrics module
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
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

#' impact_metrics Server Functions
#' @noRd 
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
              type = 'scatter', mode = 'lines',
              fill = 'tozeroy',
              line = list(color = '#4CAF50')) %>%
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
              type = 'bar',
              marker = list(color = '#2196F3')) %>%
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
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#FF9800', width = 3),
              marker = list(size = 10)) %>%
        layout(
          title = "Average Daily Income Progression (USD)",
          xaxis = list(title = "Program Phase"),
          yaxis = list(title = "Daily Income (USD)")
        )
    })
  })
}

# R/mod_field_activities.R - Field Activities Module

#' field_activities UI Function
#'
#' @description Field officer activities tracking module
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
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

#' field_activities Server Functions
#' @noRd 
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
                             prob = c(0.4, 0.4, 0.2), replace = TRUE)
      )
      
      DT::datatable(
        officer_data,
        options = list(
          pageLength = 5,
          dom = 'frtip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Performance',
          backgroundColor = DT::styleEqual(
            c("Excellent", "Good", "Average"),
            c("#C8E6C9", "#FFF9C4", "#FFCCBC")
          )
        )
    })
  })
}

