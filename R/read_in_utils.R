#' Read-In Utility Functions
#'
#' Helper functions for reading in data files, particularly for datasets
#' that span multiple Excel files with consistent naming conventions.

# Source validation utilities
source("R/validation_utils.R")


#' Get Excel files for a specific alliance
#'
#' Discovers all Excel files in a directory that match the naming pattern
#' for a given alliance (e.g., "East Midlands" or "West Midlands").
#'
#' Files are expected to follow the pattern:
#' `[Alliance]_[type]_monthly_YYYY-MM.xlsx` or
#' `[Alliance]_[type]_monthly_YYYY-MM FINAL.xlsx`
#'
#' @param path The directory path containing the Excel files.
#' @param alliance The alliance name to match (e.g., "East Midlands", "West Midlands").
#' @param type The file type to match (e.g., "provider", "commissioner"). Defaults to "provider".
#' @return A character vector of full file paths, sorted alphabetically.
#' @examples
#' get_alliance_files("data/cwt-detailed/provider", "West Midlands")
#' get_alliance_files("data/cwt-detailed/commissioner", "West Midlands", "commissioner")
#' @export
get_alliance_files <- function(path, alliance, type = "provider") {
  # Build pattern to match alliance files (with or without " FINAL" suffix)
  pattern <- paste0("^", alliance, "_", type, "_monthly_.*\\.xlsx$")

  files <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning(paste("No files found for alliance:", alliance, "in path:", path))
  }


  return(sort(files))
}


#' Read specified sheets from multiple Excel files and combine
#'
#' Reads one or more sheets from each file in a list, then combines all
#' results into a single dataframe using row-binding.
#'
#' This is useful for datasets where the same sheet structure exists across
#' multiple monthly files, and all data needs to be combined.
#'
#' @param files Character vector of file paths to read from.
#' @param sheet_names Character vector of sheet names to read from each file.
#' @param require_all_sheets If TRUE (default), stops with an error if any
#'   required sheet is missing from any file. If FALSE, issues warnings for
#'   missing sheets but continues processing.
#' @return A combined dataframe with all data from all sheets and files.
#' @examples
#' files <- get_alliance_files("data/cwt-detailed/provider", "West Midlands")
#' read_sheets_from_files(files, "28-DAY FDS")
#' read_sheets_from_files(files, c("62-DAY CLASSIC", "62-DAY SCREENING", "62-DAY UPGRADE"))
#' @export
read_sheets_from_files <- function(files, sheet_names, require_all_sheets = TRUE) {
  # Validate input files
  if (length(files) == 0) {
    stop("No files provided to read_sheets_from_files", call. = FALSE)
  }

  # Track read failures for summary
  failed_reads <- list()
  successful_reads <- 0

  # For each file, read all specified sheets and combine
  result <- map_dfr(files, function(file) {
    # Validate file exists
    if (!file.exists(file)) {
      msg <- paste("File does not exist:", basename(file))
      if (require_all_sheets) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
        failed_reads <<- c(failed_reads, list(list(file = basename(file), sheet = "N/A", reason = "File not found")))
        return(NULL)
      }
    }

    # Get available sheets in this file
    available_sheets <- get_available_sheets(file)
    if (is.null(available_sheets)) {
      msg <- paste("Could not read sheet list from:", basename(file))
      if (require_all_sheets) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
        failed_reads <<- c(failed_reads, list(list(file = basename(file), sheet = "N/A", reason = "Cannot read file")))
        return(NULL)
      }
    }

    # Read each sheet from this file
    map_dfr(sheet_names, function(sheet) {
      # Check if sheet exists before trying to read
      if (!sheet %in% available_sheets) {
        msg <- paste0(
          "Sheet '", sheet, "' not found in '", basename(file), "'. ",
          "Available sheets: ", paste(available_sheets, collapse = ", ")
        )
        if (require_all_sheets) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
          failed_reads <<- c(failed_reads, list(list(file = basename(file), sheet = sheet, reason = "Sheet not found")))
          return(NULL)
        }
      }

      tryCatch({
        df <- read_excel(file, sheet = sheet)
        successful_reads <<- successful_reads + 1
        df
      }, error = function(e) {
        msg <- paste0("Error reading sheet '", sheet, "' from '", basename(file), "': ", e$message)
        if (require_all_sheets) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
          failed_reads <<- c(failed_reads, list(list(file = basename(file), sheet = sheet, reason = e$message)))
          return(NULL)
        }
      })
    })
  })

  # Check if we got any data at all. Every path that leaves `result` empty
  # (file missing, unreadable, sheet missing, or empty) appends to
  # failed_reads first, so it's always non-empty here.
  if (is.null(result) || nrow(result) == 0) {
    total_expected <- length(files) * length(sheet_names)
    stop(
      paste0(
        "No data was successfully read from any file/sheet combination.\n",
        "Files: ", length(files), ", Sheets: ", length(sheet_names), ", Expected reads: ", total_expected, "\n",
        "Failures:\n", paste(
          sapply(failed_reads, function(f) paste0("  - ", f$file, "/", f$sheet, ": ", f$reason)),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Summary of read operation
  total_expected <- length(files) * length(sheet_names)
  if (length(failed_reads) > 0) {
    warning(
      paste0(
        "Read operation completed with ", length(failed_reads), " failures out of ", total_expected, " attempts.\n",
        "Successfully read: ", successful_reads, " sheet(s)"
      ),
      call. = FALSE
    )
  }

  result
}


#' Read and combine multiple CSV files
#'
#' Reads all CSV files from a directory matching a pattern, applies optional
#' column renaming for standardisation, and combines into a single dataframe.
#'
#' @param path The directory path containing the CSV files.
#' @param pattern Regex pattern to match files. Defaults to "\\.csv$".
#' @param col_renames Named vector of column renames (old = new). Applied if
#'   the old column exists and the new column doesn't.
#' @param require_all If TRUE (default), stops with an error if any file
#'   cannot be read. If FALSE, issues warnings but continues.
#' @return A combined dataframe with all data from matched files.
#' @examples
#' read_csv_files("data/cwt/provider")
#' read_csv_files("data/cwt/provider", col_renames = c("TOTAL" = "TOTAL.TREATED"))
#' @export
read_csv_files <- function(path, pattern = "\\.csv$", col_renames = NULL, require_all = TRUE) {
  # Validate path exists
  if (!dir.exists(path)) {
    stop(paste("Directory does not exist:", path), call. = FALSE)
  }

  files <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop(
      paste0("No CSV files matching pattern '", pattern, "' found in: ", path),
      call. = FALSE
    )
  }

  # Track failures
 failed_reads <- list()
  successful_reads <- 0

  result <- map_dfr(files, function(file) {
    tryCatch({
      df <- read.csv(file)

      # Check for empty file
      if (nrow(df) == 0) {
        msg <- paste("File is empty:", basename(file))
        if (require_all) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
          failed_reads <<- c(failed_reads, list(list(file = basename(file), reason = "Empty file")))
          return(NULL)
        }
      }

      # Apply column renames if specified
      if (!is.null(col_renames)) {
        for (old_name in names(col_renames)) {
          new_name <- col_renames[[old_name]]
          if (old_name %in% names(df) && !new_name %in% names(df)) {
            names(df)[names(df) == old_name] <- new_name
          }
        }
      }

      successful_reads <<- successful_reads + 1
      return(df)
    }, error = function(e) {
      msg <- paste0("Error reading '", basename(file), "': ", e$message)
      if (require_all) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
        failed_reads <<- c(failed_reads, list(list(file = basename(file), reason = e$message)))
        return(NULL)
      }
    })
  })

  # Check if we got any data. Every path that leaves `result` empty (file
  # unreadable or empty) appends to failed_reads first, so it's always
  # non-empty here.
  if (is.null(result) || nrow(result) == 0) {
    stop(
      paste0(
        "No data was successfully read from any CSV file.\n",
        "Path: ", path, "\n",
        "Files found: ", length(files), "\n",
        "Failures:\n", paste(
          sapply(failed_reads, function(f) paste0("  - ", f$file, ": ", f$reason)),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Summary if there were partial failures
  if (length(failed_reads) > 0) {
    warning(
      paste0(
        "CSV read completed with ", length(failed_reads), " failures out of ", length(files), " files.\n",
        "Successfully read: ", successful_reads, " file(s)"
      ),
      call. = FALSE
    )
  }

  result
}


#' Read and combine one sheet from multiple Excel files
#'
#' Reads the same sheet from every file matching a pattern in a directory,
#' skipping a fixed number of leading rows before the header (for workbooks
#' with a metadata block above the data table), and combines into a single
#' dataframe. Unlike \code{\link{read_sheets_from_files}}, this discovers
#' files itself (like \code{\link{read_csv_files}}) rather than taking a
#' pre-built file list, and supports a header row offset.
#'
#' @param path The directory path containing the Excel files.
#' @param sheet Character. The sheet name to read from each file.
#' @param pattern Regex pattern to match files. Defaults to "\\.xlsx$".
#' @param skip Number of rows to skip before the header row. Defaults to 0.
#' @param require_all If TRUE (default), stops with an error if any file
#'   cannot be read. If FALSE, issues warnings but continues.
#' @return A combined dataframe with all data from matched files, plus a
#'   `SOURCE.FILE` column giving the basename of the file each row came from.
#' @examples
#' read_excel_files("data/cwt/provider", sheet = "Provider Level Performance", skip = 14)
#' @export
read_excel_files <- function(path, sheet, pattern = "\\.xlsx$", skip = 0, require_all = TRUE) {
  # Validate path exists
  if (!dir.exists(path)) {
    stop(paste("Directory does not exist:", path), call. = FALSE)
  }

  files <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop(
      paste0("No Excel files matching pattern '", pattern, "' found in: ", path),
      call. = FALSE
    )
  }

  # Track failures
  failed_reads <- list()
  successful_reads <- 0

  result <- map_dfr(files, function(file) {
    tryCatch({
      available_sheets <- get_available_sheets(file)
      if (is.null(available_sheets) || !sheet %in% available_sheets) {
        stop(paste0("Sheet '", sheet, "' not found in '", basename(file), "'"), call. = FALSE)
      }

      df <- readxl::read_excel(file, sheet = sheet, skip = skip)

      # Check for empty file
      if (nrow(df) == 0) {
        msg <- paste("Sheet is empty:", basename(file))
        if (require_all) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
          failed_reads <<- c(failed_reads, list(list(file = basename(file), reason = "Empty sheet")))
          return(NULL)
        }
      }

      df$SOURCE.FILE <- basename(file)

      successful_reads <<- successful_reads + 1
      return(df)
    }, error = function(e) {
      msg <- paste0("Error reading '", basename(file), "': ", e$message)
      if (require_all) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
        failed_reads <<- c(failed_reads, list(list(file = basename(file), reason = e$message)))
        return(NULL)
      }
    })
  })

  # Check if we got any data. Every path that leaves `result` empty (sheet
  # missing, unreadable, or empty) appends to failed_reads first, so it's
  # always non-empty here.
  if (is.null(result) || nrow(result) == 0) {
    stop(
      paste0(
        "No data was successfully read from any Excel file.\n",
        "Path: ", path, "\n",
        "Files found: ", length(files), "\n",
        "Failures:\n", paste(
          sapply(failed_reads, function(f) paste0("  - ", f$file, ": ", f$reason)),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Summary if there were partial failures
  if (length(failed_reads) > 0) {
    warning(
      paste0(
        "Excel read completed with ", length(failed_reads), " failures out of ", length(files), " files.\n",
        "Successfully read: ", successful_reads, " file(s)"
      ),
      call. = FALSE
    )
  }

  result
}
