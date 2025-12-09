view_program <- function() {
  # Map menu numbers to file paths
  # Map menu numbers to file paths
  # Use system.file to find files inside the installed package
  # "programs" matches the folder name inside "inst"
  programs <- list(
    "1" = system.file("programs", "prog1.R", package = "RLabPrograms"),
    "2" = system.file("programs", "prog2.R", package = "RLabPrograms"),
    "3" = system.file("programs", "prog3.R", package = "RLabPrograms"),
    "4" = system.file("programs", "prog4.R", package = "RLabPrograms"),
    "5" = system.file("programs", "prog5.R", package = "RLabPrograms"),
    "6" = system.file("programs", "prog6.R", package = "RLabPrograms"),
    "7" = system.file("programs", "prog7.R", package = "RLabPrograms"),
    "8" = system.file("programs", "prog8.R", package = "RLabPrograms"),
    "9" = system.file("programs", "prog9.R", package = "RLabPrograms")
  )

  while (TRUE) {
    cat("
======================================
   Program Index
======================================

1. Arithmetic Operation, Looping Statements,
   Conditional Statements

2. Creating and Manipulating Data Structures

3. Basic Statistical Operations on Open-Source Datasets

4. Data Import, Cleaning, and Export with Advanced Data Wrangling

5. Advanced Data Manipulation with dplyr and Complex Grouping

6. Data Visualization with ggplot2 and Customizations

7. Linear and Multiple Regression Analysis
   with Interaction Terms

8. K-Means Clustering and PCA for Dimensionality Reduction

9. Time Series Analysis using ARIMA and Seasonal Decomposition

======================================
")


    cat("0. Exit\n")
    choice <- readline("\nEnter program number to view (or 0 to exit): ")

    if (choice == "0") {
      cat("Exiting...\n")
      break
    } else if (choice %in% names(programs)) {
      file <- programs[[choice]]
      cat("\n------------------------------------------------\n")
      cat(sprintf(" CODE FOR PROGRAM %s (%s)", choice, file))
      cat("\n------------------------------------------------\n")
      code <- readLines(file, warn = FALSE)
      cat(paste(code, collapse = "\n"))
      cat("\n------------------------------------------------\n")
      readline("\nPress Enter to return to menu...")
    } else {
      cat("Invalid selection. Please try again.\n")
    }
  }
}
