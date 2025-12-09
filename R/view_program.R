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
