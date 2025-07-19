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


