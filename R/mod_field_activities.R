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


