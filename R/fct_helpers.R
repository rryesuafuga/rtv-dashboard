


# R/fct_helpers.R - Helper functions for RTV Dashboard

#' Calculate vulnerability score
#' 
#' @param data Household data frame
#' @return Numeric vector of vulnerability scores
#' @export
calculate_vulnerability_score <- function(data) {
  score <- 100 - (
    data$has_safe_water_access * 20 +
      data$has_latrine * 15 +
      (data$current_income_usd_daily / 2) * 30 +
      data$uses_improved_seeds * 15 +
      data$has_vsla_membership * 10 +
      (!data$female_headed) * 10
  )
  
  pmax(0, pmin(100, score))
}

#' Format currency values
#' 
#' @param value Numeric value
#' @param currency Currency symbol
#' @return Formatted string
#' @export
format_currency <- function(value, currency = "$") {
  paste0(currency, formatC(value, format = "f", digits = 2, big.mark = ","))
}

#' Calculate ROI percentage
#' 
#' @param cost Program cost
#' @param value Value generated
#' @return ROI percentage
#' @export
calculate_roi <- function(cost, value) {
  ((value - cost) / cost) * 100
}

#' Create color palette for vulnerability scores
#' 
#' @param n Number of colors
#' @return Color palette function
#' @export
vulnerability_palette <- function(n = 100) {
  colorRampPalette(c("#4CAF50", "#FFEB3B", "#FF9800", "#F44336"))(n)
}

#' Generate summary statistics
#' 
#' @param data Data frame
#' @param group_var Grouping variable
#' @return Summary data frame
#' @export
generate_summary_stats <- function(data, group_var = NULL) {
  if (!is.null(group_var)) {
    data <- data %>% group_by(across(all_of(group_var)))
  }
  
  data %>%
    summarise(
      n = n(),
      avg_vulnerability = mean(vulnerability_score, na.rm = TRUE),
      avg_income = mean(current_income_usd_daily, na.rm = TRUE),
      water_access_rate = mean(has_safe_water_access, na.rm = TRUE) * 100,
      vsla_rate = mean(has_vsla_membership, na.rm = TRUE) * 100,
      .groups = 'drop'
    )
}

#' Cache reactive data with timeout
#' 
#' @param expr Expression to cache
#' @param timeout_seconds Cache timeout in seconds
#' @return Cached reactive
#' @export
cached_reactive <- function(expr, timeout_seconds = 3600) {
  cache_time <- reactiveVal(Sys.time() - timeout_seconds - 1)
  cache_value <- reactiveVal(NULL)
  
  reactive({
    if (difftime(Sys.time(), cache_time(), units = "secs") > timeout_seconds) {
      cache_value(expr)
      cache_time(Sys.time())
    }
    cache_value()
  })
}

#' Create download handler for data
#' 
#' @param data Reactive data
#' @param filename Base filename
#' @return Download handler
#' @export
create_download_handler <- function(data, filename = "rtv_data") {
  downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(data(), file)
    }
  )
}

#' Validate uploaded file
#' 
#' @param file File input
#' @param max_size_mb Maximum file size in MB
#' @return TRUE if valid, error message if not
#' @export
validate_upload <- function(file, max_size_mb = 30) {
  if (is.null(file)) {
    return("No file selected")
  }
  
  # Check file size
  if (file$size > max_size_mb * 1024^2) {
    return(paste("File too large. Maximum size:", max_size_mb, "MB"))
  }
  
  # Check file extension
  ext <- tools::file_ext(file$name)
  if (!ext %in% c("csv", "xlsx", "xls")) {
    return("Invalid file type. Please upload CSV or Excel files.")
  }
  
  return(TRUE)
}

#' Create notification with auto-dismiss
#' 
#' @param message Notification message
#' @param type Notification type
#' @param duration Duration in seconds
#' @export
notify <- function(message, type = "message", duration = 3) {
  showNotification(
    message,
    type = type,
    duration = duration,
    closeButton = TRUE
  )
}

#' Format large numbers
#' 
#' @param x Numeric value
#' @param digits Number of digits
#' @return Formatted string
#' @export
format_number <- function(x, digits = 0) {
  if (abs(x) >= 1e6) {
    paste0(round(x / 1e6, digits), "M")
  } else if (abs(x) >= 1e3) {
    paste0(round(x / 1e3, digits), "K")
  } else {
    as.character(round(x, digits))
  }
}

#' Calculate percentage change
#' 
#' @param old_value Previous value
#' @param new_value Current value
#' @return Percentage change
#' @export
percent_change <- function(old_value, new_value) {
  if (old_value == 0) return(NA)
  ((new_value - old_value) / old_value) * 100
}

#' Create progress indicator
#' 
#' @param session Shiny session
#' @param message Progress message
#' @param detail Progress detail
#' @return Progress object
#' @export
create_progress <- function(session, message = "Processing...", detail = NULL) {
  progress <- Progress$new(session, min = 0, max = 1)
  progress$set(message = message, detail = detail)
  progress
}

#' Safe division
#' 
#' @param numerator Numerator
#' @param denominator Denominator
#' @param na_value Value to return if denominator is 0
#' @return Result of division
#' @export
safe_divide <- function(numerator, denominator, na_value = NA) {
  ifelse(denominator == 0, na_value, numerator / denominator)
}

#' Get date range for filtering
#' 
#' @param period Period name
#' @return List with start and end dates
#' @export
get_date_range <- function(period = "last_month") {
  end_date <- Sys.Date()
  
  start_date <- switch(
    period,
    "last_week" = end_date - 7,
    "last_month" = end_date - 30,
    "last_quarter" = end_date - 90,
    "last_year" = end_date - 365,
    "ytd" = as.Date(paste0(year(end_date), "-01-01")),
    "all_time" = as.Date("2020-01-01")
  )
  
  list(start = start_date, end = end_date)
}

# R/utils_helpers.R - Utility functions

#' Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Safe division helper
#' @export
safe_div <- function(x, y, default = 0) {
  ifelse(y == 0, default, x / y)
}

# R/app_config.R - Configuration settings

#' Access application configuration
#' 
#' @param value Configuration value to retrieve
#' @param config Configuration name
#' @param use_parent Whether to use parent directory
#' @return Configuration value
#' @export
get_golem_config <- function(
    value,
    config = Sys.getenv("GOLEM_CONFIG_ACTIVE", "default"),
    use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}

