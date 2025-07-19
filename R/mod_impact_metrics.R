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


