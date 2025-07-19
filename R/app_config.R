

# R/app_config.R - Application configuration

#' Access application configuration
#' 
#' @param value Configuration value to retrieve
#' @param config Configuration name
#' @param use_parent Whether to use parent directory
#' @return Configuration value
#' @export
get_golem_config <- function(
    value,
    config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
    use_parent = TRUE,
    file = "inst/golem-config.yml"
) {
  
  # If config file doesn't exist, return default values
  if (!file.exists(file)) {
    default_config <- list(
      golem_name = "rtvdashboard",
      golem_version = "0.0.0.9000",
      app_prod = FALSE,
      app_title = "RTV Data Science Dashboard"
    )
    
    return(default_config[[value]] %||% value)
  }
  
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' Read configuration file
#' 
#' @param file Path to config file
#' @param config Configuration to use
#' @return List of configuration values
get_config <- function(
    file = "inst/config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE", "default")
) {
  
  # Default configuration if file doesn't exist
  if (!file.exists(file)) {
    return(list(
      datapath = "data/",
      cache_timeout = 3600,
      max_upload_size = 30,
      theme = "green",
      debug = FALSE
    ))
  }
  
  config::get(config = config, file = file)
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Get package version
#' 
#' @return Package version string
#' @export
get_package_version <- function() {
  desc <- read.dcf("DESCRIPTION")
  version <- desc[, "Version"]
  return(as.character(version))
}

#' Check if running in production
#' 
#' @return Logical indicating if app is in production
#' @export
is_production <- function() {
  Sys.getenv("R_CONFIG_ACTIVE") == "production" ||
    Sys.getenv("SHINY_ENV") == "production" ||
    get_golem_config("app_prod") == TRUE
}

#' Get app title
#' 
#' @return Application title
#' @export
get_app_title <- function() {
  get_golem_config("app_title", config = "default") %||% "RTV Data Science Dashboard"
}

