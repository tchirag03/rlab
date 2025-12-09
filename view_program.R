view_program <- function() {
  # Map menu numbers to file paths
  programs <- list(
    "1" = "programs/prog1.R",
    "2" = "programs/prog2.R",
    "3" = "programs/prog3.R",
    "4" = "programs/prog4.R",
    "5" = "programs/prog5.R",
    "6" = "programs/prog6.R",
    "7" = "programs/prog7.R",
    "8" = "programs/prog8.R",
    "9" = "programs/prog9.R"
  )

  while (TRUE) {
    cat("\n==============================\n")
    cat("      R LAB PROGRAMS MENU     \n")
    cat("==============================\n")
    cat("Available Programs:\n")

    for (num in sort(as.numeric(names(programs)))) {
      file <- programs[[as.character(num)]]
      first_line <- tryCatch(readLines(file, n = 1, warn = FALSE), error = function(e) "")      
      desc <- if (grepl("^#", trimws(first_line))) {
        trimws(sub("^#", "", trimws(first_line)))
      } else {
        "No description"
      }
      cat(sprintf("%d. %s\n", num, desc))
    }

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
