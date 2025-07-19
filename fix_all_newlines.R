

# fix_all_newlines.R - Comprehensive fix for all newline warnings

cat("Fixing newline warnings in all R files...\n\n")

# Function to ensure file ends with newline
ensure_newline <- function(filepath) {
  if (file.exists(filepath)) {
    # Read file content
    content <- readLines(filepath, warn = FALSE)
    
    # Write back with proper newline
    writeLines(content, filepath)
    
    # Double-check by reading file size
    file_size <- file.info(filepath)$size
    if (file_size > 0) {
      # Read last character
      con <- file(filepath, "rb")
      seek(con, file_size - 1)
      last_char <- readChar(con, 1)
      close(con)
      
      # If still no newline, force add one
      if (last_char != "\n") {
        cat("\n", file = filepath, append = TRUE)
      }
    }
    
    return(TRUE)
  }
  return(FALSE)
}

# Get all R files
r_files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
  list.files(".", pattern = "\\.R$", full.names = TRUE)
)

# Remove duplicates and non-existent files
r_files <- unique(r_files[file.exists(r_files)])

# Fix each file
cat("Found", length(r_files), "R files to check\n\n")

for (file in r_files) {
  if (ensure_newline(file)) {
    cat("✓ Fixed:", file, "\n")
  }
}

# Specifically check the files mentioned in the warnings
problem_files <- c(
  "R/mod_vulnerability_scoring.R",
  "R/mod_climate_risk.R"
)

cat("\nDouble-checking problem files...\n")
for (file in problem_files) {
  if (file.exists(file)) {
    # Force fix by reading and writing
    content <- readLines(file, warn = FALSE)
    con <- file(file, "w")
    writeLines(content, con)
    close(con)
    
    # Add extra newline to be sure
    cat("\n", file = file, append = TRUE)
    cat("✓ Force-fixed:", file, "\n")
  }
}

cat("\n✓ All files should now have proper line endings!\n")
cat("\nYou can now run: shiny::runApp()\n")

# Test that files are fixed
cat("\nTesting files...\n")
test_files <- problem_files[file.exists(problem_files)]
for (file in test_files) {
  tryCatch({
    test_read <- readLines(file)
    cat("✓", file, "- OK\n")
  }, warning = function(w) {
    cat("⚠", file, "- Still has warning\n")
  })
}
