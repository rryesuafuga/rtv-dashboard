# R/app_server.R - Server Configuration

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @noRd
app_server <- function(input, output, session) {
  
  # Set options for better performance
  options(shiny.maxRequestSize = 30*1024^2) # 30MB max file size
  
  # Load data (with caching for performance)
  datasets <- reactiveValues(
    household = NULL,
    climate = NULL,
    crops = NULL,
    impact = NULL,
    field = NULL,
    worldbank = NULL
  )
  
  # Load data on app start
  observe({
    withProgress(message = "Loading datasets...", value = 0, {
      
      incProgress(0.2, detail = "Loading household data")
      tryCatch({
        datasets$household <- read_csv("data/household_vulnerability_data.csv", 
                                      show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading household data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.2, detail = "Loading climate data")
      tryCatch({
        datasets$climate <- read_csv("data/climate_risk_data.csv", 
                                    show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading climate data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.2, detail = "Loading crop diagnostics")
      tryCatch({
        datasets$crops <- read_csv("data/crop_diagnostics_data.csv", 
                                  show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading crop data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.2, detail = "Loading impact metrics")
      tryCatch({
        datasets$impact <- read_csv("data/program_impact_metrics.csv", 
                                   show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading impact data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.1, detail = "Loading field activities")
      tryCatch({
        datasets$field <- read_csv("data/field_officer_activity.csv", 
                                  show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading field data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
      
      incProgress(0.1, detail = "Loading World Bank data")
      tryCatch({
        datasets$worldbank <- read_csv("agricultureandruraldevelopment_uga.csv", 
                                      show_col_types = FALSE)
      }, error = function(e) {
        showNotification(paste("Error loading World Bank data:", e$message), 
                        type = "error", duration = 10)
        NULL
      })
    })
  })
  
  # Call module servers
  mod_overview_server("overview_1", datasets)
  mod_vulnerability_scoring_server("vulnerability_1", datasets)
  mod_climate_risk_server("climate_1", datasets)
  mod_crop_diagnostics_server("crops_1", datasets)
  mod_impact_metrics_server("impact_1", datasets)
  mod_field_activities_server("field_1", datasets)
  
  # World Bank data table
  output$worldbank_table <- DT::renderDataTable({
    req(datasets$worldbank)
    
    DT::datatable(
      datasets$worldbank,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ),
      filter = "top",
      class = "cell-border stripe hover",
      extensions = "Buttons"
    )
  })
  
  # File upload handling
  output$upload_status <- renderPrint({
    if (is.null(input$file_upload)) {
      "No file selected"
    } else {
      paste("File ready:", input$file_upload$name)
    }
  })
  
  observeEvent(input$upload_btn, {
    req(input$file_upload)
    
    showNotification(
      paste("Processing", input$file_upload$name),
      type = "message",
      duration = 3
    )
    
    # Process file based on type
    tryCatch({
      if (input$file_type == "csv") {
        # Read CSV
        data <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
      } else {
        # Read Excel
        data <- readxl::read_excel(input$file_upload$datapath)
      }
      
      showNotification(
        paste("Successfully loaded", nrow(data), "rows"),
        type = "success",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # Global search functionality
  observeEvent(input$search_global, {
    search_term <- input$search_global
    
    if (nchar(search_term) > 2) {
      # Implement search across all datasets
      showNotification(
        paste("Searching for:", search_term),
        type = "message",
        duration = 2
      )
    }
  })
  
  # Session info for debugging
  output$session_info <- renderPrint({
    sessionInfo()
  })
}


