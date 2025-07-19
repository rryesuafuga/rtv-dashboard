

# R/mod_climate_risk.R - Climate Risk Engine Module

#' climate_risk UI Function
#'
#' @description Climate risk assessment and prediction module
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
mod_climate_risk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Time and location controls
    fluidRow(
      box(
        width = 12,
        title = "Climate Analysis Parameters",
        status = "info",
        solidHeader = FALSE,
        
        fluidRow(
          column(4,
                 dateRangeInput(ns("date_range"),
                                "Select Date Range:",
                                start = Sys.Date() - 365,
                                end = Sys.Date(),
                                max = Sys.Date())
          ),
          column(4,
                 selectInput(ns("climate_variable"),
                             "Climate Variable:",
                             choices = c("Rainfall" = "rainfall_mm",
                                         "Temperature" = "avg_temp_c",
                                         "Drought Risk" = "drought_risk"),
                             selected = "rainfall_mm")
          ),
          column(4,
                 br(),
                 actionButton(ns("analyze_climate"),
                              "Analyze Climate Patterns",
                              icon = icon("cloud"),
                              class = "btn-primary btn-block")
          )
        )
      )
    ),
    
    # Main visualizations
    fluidRow(
      # Spatial map
      box(
        width = 8,
        title = "Spatial Climate Risk Map",
        status = "warning",
        solidHeader = TRUE,
        leafletOutput(ns("climate_map"), height = 500)
      ),
      
      # Risk indicators
      box(
        width = 4,
        title = "Current Risk Indicators",
        status = "danger",
        solidHeader = TRUE,
        
        valueBoxOutput(ns("drought_risk_box"), width = 12),
        br(),
        valueBoxOutput(ns("rainfall_anomaly_box"), width = 12),
        br(),
        valueBoxOutput(ns("temp_anomaly_box"), width = 12)
      )
    ),
    
    # Time series and predictions
    fluidRow(
      # Historical trends
      box(
        width = 6,
        title = "Historical Climate Trends",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("climate_timeseries"), height = 400)
      ),
      
      # Seasonal patterns
      box(
        width = 6,
        title = "Seasonal Patterns & Predictions",
        status = "success",
        solidHeader = TRUE,
        plotlyOutput(ns("seasonal_forecast"), height = 400)
      )
    ),
    
    # Agricultural impact analysis
    fluidRow(
      box(
        width = 12,
        title = "Agricultural Impact Analysis",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        tabsetPanel(
          tabPanel("Crop Suitability",
                   br(),
                   plotlyOutput(ns("crop_suitability"), height = 400)
          ),
          tabPanel("Risk Calendar",
                   br(),
                   plotlyOutput(ns("risk_calendar"), height = 400)
          ),
          tabPanel("Climate Anomalies",
                   br(),
                   DT::dataTableOutput(ns("anomaly_table"))
          )
        )
      )
    ),
    
    # 3D visualization section
    fluidRow(
      box(
        width = 12,
        title = "Advanced Climate Visualization (3D)",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        tags$div(
          id = ns("climate_3d_container"),
          style = "height: 600px; position: relative;",
          
          # Controls for 3D view
          tags$div(
            style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
            actionButton(ns("reset_3d_view"), "Reset View", 
                         class = "btn-sm btn-default"),
            actionButton(ns("animate_3d"), "Animate", 
                         class = "btn-sm btn-info")
          ),
          
          # 3D plot container
          tags$div(
            id = ns("climate_3d_plot"),
            style = "width: 100%; height: 100%;"
          )
        )
      )
    )
  )
}

#' climate_risk Server Functions
#' @noRd 
mod_climate_risk_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive for filtered climate data
    climate_filtered <- reactive({
      req(datasets$climate, input$date_range)
      
      datasets$climate %>%
        filter(date >= input$date_range[1],
               date <= input$date_range[2])
    })
    
    # Climate map
    output$climate_map <- renderLeaflet({
      req(climate_filtered())
      
      # Get latest data for each location
      map_data <- climate_filtered() %>%
        group_by(lat, lon) %>%
        filter(date == max(date)) %>%
        ungroup()
      
      # Select variable for display
      var_col <- input$climate_variable
      var_name <- switch(var_col,
                         "rainfall_mm" = "Rainfall (mm)",
                         "avg_temp_c" = "Temperature (°C)",
                         "drought_risk" = "Drought Risk"
      )
      
      # Create color palette
      pal <- colorNumeric(
        palette = switch(var_col,
                         "rainfall_mm" = "Blues",
                         "avg_temp_c" = "RdYlBu",
                         "drought_risk" = "RdYlRd"
        ),
        domain = map_data[[var_col]],
        reverse = (var_col == "avg_temp_c")
      )
      
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addRectangles(
          lng1 = ~lon - 0.025, lng2 = ~lon + 0.025,
          lat1 = ~lat - 0.025, lat2 = ~lat + 0.025,
          fillColor = ~pal(get(var_col)),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          color = "white",
          popup = ~paste0(
            "<strong>Location:</strong> ", round(lat, 3), ", ", round(lon, 3), "<br>",
            "<strong>", var_name, ":</strong> ", round(get(var_col), 2), "<br>",
            "<strong>Date:</strong> ", format(date, "%Y-%m-%d")
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~get(var_col),
          title = var_name,
          opacity = 0.8
        ) %>%
        addScaleBar(position = "bottomleft") %>%
        setView(lng = 30.75, lat = -0.3, zoom = 9)
    })
    
    # Risk indicator boxes
    output$drought_risk_box <- renderValueBox({
      req(climate_filtered())
      
      current_risk <- climate_filtered() %>%
        filter(date == max(date)) %>%
        summarise(avg_risk = mean(drought_risk, na.rm = TRUE)) %>%
        pull(avg_risk)
      
      valueBox(
        value = paste0(round(current_risk * 100), "%"),
        subtitle = "Current Drought Risk",
        icon = icon("exclamation-triangle"),
        color = if (current_risk > 0.7) "red" else if (current_risk > 0.4) "yellow" else "green"
      )
    })
    
    output$rainfall_anomaly_box <- renderValueBox({
      req(climate_filtered())
      
      # Calculate anomaly from historical average
      current_month <- month(max(climate_filtered()$date))
      
      historical_avg <- climate_filtered() %>%
        filter(month == current_month) %>%
        summarise(avg = mean(rainfall_mm, na.rm = TRUE)) %>%
        pull(avg)
      
      current_rainfall <- climate_filtered() %>%
        filter(date == max(date)) %>%
        summarise(avg = mean(rainfall_mm, na.rm = TRUE)) %>%
        pull(avg)
      
      anomaly_pct <- ((current_rainfall - historical_avg) / historical_avg) * 100
      
      valueBox(
        value = paste0(ifelse(anomaly_pct > 0, "+", ""), round(anomaly_pct), "%"),
        subtitle = "Rainfall Anomaly",
        icon = icon("cloud-rain"),
        color = if (abs(anomaly_pct) > 30) "red" else if (abs(anomaly_pct) > 15) "yellow" else "green"
      )
    })
    
    output$temp_anomaly_box <- renderValueBox({
      req(climate_filtered())
      
      # Calculate temperature anomaly
      historical_avg_temp <- mean(climate_filtered()$avg_temp_c, na.rm = TRUE)
      
      current_temp <- climate_filtered() %>%
        filter(date == max(date)) %>%
        summarise(avg = mean(avg_temp_c, na.rm = TRUE)) %>%
        pull(avg)
      
      temp_anomaly <- current_temp - historical_avg_temp
      
      valueBox(
        value = paste0(ifelse(temp_anomaly > 0, "+", ""), round(temp_anomaly, 1), "°C"),
        subtitle = "Temperature Anomaly",
        icon = icon("thermometer-half"),
        color = if (abs(temp_anomaly) > 2) "red" else if (abs(temp_anomaly) > 1) "yellow" else "green"
      )
    })
    
    # Climate time series
    output$climate_timeseries <- renderPlotly({
      req(climate_filtered())
      
      # Aggregate by month
      monthly_data <- climate_filtered() %>%
        group_by(date) %>%
        summarise(
          avg_value = mean(get(input$climate_variable), na.rm = TRUE),
          .groups = 'drop'
        )
      
      var_name <- switch(input$climate_variable,
                         "rainfall_mm" = "Rainfall (mm)",
                         "avg_temp_c" = "Temperature (°C)",
                         "drought_risk" = "Drought Risk Index"
      )
      
      plot_ly(monthly_data, x = ~date, y = ~avg_value,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#2196F3', width = 2),
              marker = list(size = 5),
              hovertemplate = paste0(var_name, ': %{y:.2f}<br>Date: %{x}<extra></extra>')) %>%
        add_trace(y = ~{
          if(nrow(monthly_data) > 10) {
            predict(loess(avg_value ~ as.numeric(date), data = monthly_data, span = 0.5))
          } else {
            avg_value
          }
        },
        type = 'scatter', mode = 'lines',
        line = list(color = 'rgba(255, 152, 0, 0.8)', dash = 'dash'),
        name = 'Trend',
        hovertemplate = 'Trend: %{y:.2f}<extra></extra>') %>%
        layout(
          xaxis = list(title = "Date", rangeslider = list(visible = TRUE)),
          yaxis = list(title = var_name),
          hovermode = 'x unified',
          showlegend = TRUE,
          font = list(family = "Inter, sans-serif")
        )
    })
    
    # Seasonal forecast
    output$seasonal_forecast <- renderPlotly({
      req(climate_filtered())
      
      # Calculate seasonal patterns
      seasonal_data <- climate_filtered() %>%
        mutate(month_name = month.abb[month]) %>%
        group_by(month, month_name) %>%
        summarise(
          avg_rainfall = mean(rainfall_mm, na.rm = TRUE),
          avg_temp = mean(avg_temp_c, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(month)
      
      # Add forecast (simplified - in reality would use proper forecasting)
      future_months <- data.frame(
        month = c(month(Sys.Date()) + 1:3) %% 12,
        month_name = month.abb[c(month(Sys.Date()) + 1:3) %% 12]
      ) %>%
        mutate(
          avg_rainfall = seasonal_data$avg_rainfall[match(month, seasonal_data$month)] * 
            runif(3, 0.9, 1.1),
          avg_temp = seasonal_data$avg_temp[match(month, seasonal_data$month)] +
            runif(3, -0.5, 0.5),
          forecast = TRUE
        )
      
      seasonal_data$forecast <- FALSE
      combined_data <- rbind(seasonal_data, future_months)
      
      # Create dual-axis plot
      plot_ly(combined_data) %>%
        add_bars(x = ~month_name, y = ~avg_rainfall,
                 name = "Rainfall",
                 marker = list(color = ifelse(combined_data$forecast, 
                                              'rgba(33, 150, 243, 0.5)', 
                                              '#2196F3')),
                 hovertemplate = 'Rainfall: %{y:.1f} mm<extra></extra>') %>%
        add_lines(x = ~month_name, y = ~avg_temp,
                  name = "Temperature",
                  yaxis = "y2",
                  line = list(color = '#FF5722', width = 3),
                  mode = 'lines+markers',
                  marker = list(size = 8),
                  hovertemplate = 'Temperature: %{y:.1f}°C<extra></extra>') %>%
        layout(
          xaxis = list(title = "Month", categoryorder = "array",
                       categoryarray = month.abb),
          yaxis = list(title = "Rainfall (mm)", side = "left"),
          yaxis2 = list(title = "Temperature (°C)", 
                        overlaying = "y", side = "right"),
          hovermode = 'x unified',
          barmode = 'group',
          font = list(family = "Inter, sans-serif"),
          annotations = list(
            list(x = future_months$month_name[1], y = 1, 
                 text = "Forecast →", showarrow = FALSE,
                 xref = "x", yref = "paper", yanchor = "bottom")
          )
        )
    })
    
    # Crop suitability analysis
    output$crop_suitability <- renderPlotly({
      req(climate_filtered())
      
      # Define optimal conditions for crops
      crop_optimal <- data.frame(
        crop = c("Banana", "Coffee", "Maize", "Beans"),
        optimal_rain_min = c(100, 120, 80, 60),
        optimal_rain_max = c(200, 180, 150, 120),
        optimal_temp_min = c(20, 18, 18, 16),
        optimal_temp_max = c(30, 25, 30, 28)
      )
      
      # Calculate suitability scores
      current_conditions <- climate_filtered() %>%
        filter(date == max(date)) %>%
        summarise(
          avg_rainfall = mean(rainfall_mm, na.rm = TRUE),
          avg_temp = mean(avg_temp_c, na.rm = TRUE)
        )
      
      crop_optimal <- crop_optimal %>%
        mutate(
          rain_score = pmax(0, pmin(100, 
                                    100 - abs(current_conditions$avg_rainfall - 
                                                (optimal_rain_min + optimal_rain_max)/2) / 
                                      ((optimal_rain_max - optimal_rain_min)/2) * 50)),
          temp_score = pmax(0, pmin(100,
                                    100 - abs(current_conditions$avg_temp - 
                                                (optimal_temp_min + optimal_temp_max)/2) / 
                                      ((optimal_temp_max - optimal_temp_min)/2) * 50)),
          overall_suitability = (rain_score + temp_score) / 2
        )
      
      plot_ly(crop_optimal, x = ~crop, y = ~overall_suitability,
              type = 'bar',
              marker = list(
                color = ~overall_suitability,
                colorscale = list(
                  c(0, '#F44336'),
                  c(0.5, '#FFC107'),
                  c(1, '#4CAF50')
                ),
                showscale = FALSE
              ),
              text = ~paste0(round(overall_suitability), "%"),
              textposition = 'outside',
              hovertemplate = paste0(
                'Crop: %{x}<br>',
                'Suitability: %{y:.1f}%<br>',
                'Rain Score: ', round(crop_optimal$rain_score, 1), '%<br>',
                'Temp Score: ', round(crop_optimal$temp_score, 1), '%',
                '<extra></extra>'
              )) %>%
        layout(
          xaxis = list(title = "Crop Type"),
          yaxis = list(title = "Suitability Score (%)", range = c(0, 110)),
          showlegend = FALSE,
          font = list(family = "Inter, sans-serif")
        )
    })
    
    # Risk calendar heatmap
    output$risk_calendar <- renderPlotly({
      req(climate_filtered())
      
      # Create calendar data
      calendar_data <- climate_filtered() %>%
        mutate(
          week = week(date),
          weekday = wday(date, label = TRUE),
          risk_level = case_when(
            drought_risk > 0.7 ~ 3,
            drought_risk > 0.4 ~ 2,
            drought_risk > 0.2 ~ 1,
            TRUE ~ 0
          )
        ) %>%
        filter(year(date) == year(max(date)))
      
      plot_ly(
        calendar_data,
        x = ~week,
        y = ~weekday,
        z = ~risk_level,
        type = "heatmap",
        colorscale = list(
          c(0, '#4CAF50'),
          c(0.33, '#FFEB3B'),
          c(0.67, '#FF9800'),
          c(1, '#F44336')
        ),
        hovertemplate = paste0(
          'Date: %{text}<br>',
          'Risk Level: %{z}<extra></extra>'
        ),
        text = ~format(date, "%Y-%m-%d"),
        showscale = FALSE
      ) %>%
        layout(
          xaxis = list(title = "Week of Year", dtick = 4),
          yaxis = list(title = "Day of Week", 
                       categoryorder = "array",
                       categoryarray = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
          font = list(family = "Inter, sans-serif")
        )
    })
    
    # Anomaly table
    output$anomaly_table <- DT::renderDataTable({
      req(climate_filtered())
      
      # Identify anomalies
      anomalies <- climate_filtered() %>%
        filter(is_anomaly) %>%
        select(date, lat, lon, rainfall_mm, avg_temp_c, drought_risk) %>%
        arrange(desc(date)) %>%
        head(100)
      
      DT::datatable(
        anomalies,
        options = list(
          pageLength = 10,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        class = 'cell-border stripe hover compact',
        rownames = FALSE
      ) %>%
        DT::formatRound(c("lat", "lon"), 3) %>%
        DT::formatRound(c("rainfall_mm", "avg_temp_c", "drought_risk"), 2) %>%
        DT::formatDate("date", "toLocaleDateString")
    })
    
    # 3D Climate Visualization (using custom JavaScript)
    observeEvent(input$analyze_climate, {
      # Send data to JavaScript for 3D visualization
      session$sendCustomMessage(
        type = "render3DClimate",
        message = list(
          data = climate_filtered() %>%
            sample_n(min(1000, nrow(.))) %>%
            select(lat, lon, date, rainfall_mm, avg_temp_c, drought_risk) %>%
            mutate(date_numeric = as.numeric(date)),
          container = ns("climate_3d_plot")
        )
      )
    })
    
    # Reset 3D view
    observeEvent(input$reset_3d_view, {
      session$sendCustomMessage(type = "reset3DView", message = ns("climate_3d_plot"))
    })
    
    # Animate 3D view
    observeEvent(input$animate_3d, {
      session$sendCustomMessage(type = "animate3D", message = ns("climate_3d_plot"))
    })
  })
}


