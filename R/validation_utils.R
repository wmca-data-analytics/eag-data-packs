#' Validation Utility Functions
#'
#' Helper functions for validating data at various stages of processing.
#' These functions provide consistent error handling and informative messages
#' to prevent silent failures cascading through the pipeline.


#' Validate that a dataframe is not empty
#'
#' Checks that a dataframe has at least one row. If empty, stops execution
#' with an informative error message including the context.
#'
#' @param df A dataframe to validate.
#' @param context A string describing where this validation is occurring
#'   (e.g., "FDS filtering for Breast tumour type").
#' @return The input dataframe (unchanged) if validation passes.
#' @examples
#' df <- validate_df_not_empty(filtered_df, "FDS filtering for Breast")
#' @export
validate_df_not_empty <- function(df, context) {
  if (is.null(df)) {
    stop(paste("NULL dataframe in", context), call. = FALSE)
  }
  if (nrow(df) == 0) {
    stop(paste("Empty dataframe (0 rows) in", context), call. = FALSE)
  }
  df
}


#' Validate that a dataframe is not empty (soft version)
#'
#' Like validate_df_not_empty but issues a warning instead of stopping.
#' Returns NULL if the dataframe is empty, allowing calling code to handle
#' the case gracefully.
#'
#' @param df A dataframe to validate.
#' @param context A string describing where this validation is occurring.
#' @return The input dataframe if valid, or NULL with a warning if empty.
#' @examples
#' df <- validate_df_not_empty_warn(filtered_df, "optional staging data")
#' if (!is.null(df)) { process(df) }
#' @export
validate_df_not_empty_warn <- function(df, context) {
  if (is.null(df)) {
    warning(paste("NULL dataframe in", context), call. = FALSE)
    return(NULL)
  }
  if (nrow(df) == 0) {
    warning(paste("Empty dataframe (0 rows) in", context), call. = FALSE)
    return(NULL)
  }
  df
}


#' Validate that an Excel sheet exists in a workbook
#'
#' Checks that a sheet name exists in the Excel file before attempting to read.
#' Returns TRUE if the sheet exists, FALSE otherwise.
#'
#' @param file_path Path to the Excel file.
#' @param sheet_name Name of the sheet to check.
#' @return TRUE if sheet exists, FALSE otherwise.
#' @examples
#' if (validate_sheet_exists("data.xlsx", "28-DAY FDS")) {
#'   data <- read_excel("data.xlsx", sheet = "28-DAY FDS")
#' }
#' @export
validate_sheet_exists <- function(file_path, sheet_name) {
  tryCatch({
    available_sheets <- readxl::excel_sheets(file_path)
    sheet_name %in% available_sheets
  }, error = function(e) {
    warning(
      paste("Could not read sheets from file:", basename(file_path), "-", e$message),
      call. = FALSE
    )
    FALSE
  })
}


#' Get available sheets in an Excel file
#'
#' Safely retrieves the list of sheet names from an Excel file.
#' Returns NULL with a warning if the file cannot be read.
#'
#' @param file_path Path to the Excel file.
#' @return Character vector of sheet names, or NULL if file cannot be read.
#' @examples
#' sheets <- get_available_sheets("data.xlsx")
#' @export
get_available_sheets <- function(file_path) {
  tryCatch({
    readxl::excel_sheets(file_path)
  }, error = function(e) {
    warning(
      paste("Could not read sheets from file:", basename(file_path), "-", e$message),
      call. = FALSE
    )
    NULL
  })
}


#' Validate that files exist
#'
#' Checks that all specified file paths exist. Stops execution with an
#' informative error listing any missing files.
#'
#' @param file_paths A character vector of file paths to check.
#' @param context A string describing where this validation is occurring.
#' @return The input file paths (unchanged) if all exist.
#' @examples
#' files <- validate_files_exist(my_files, "CWT detailed provider data")
#' @export
validate_files_exist <- function(file_paths, context) {
  if (length(file_paths) == 0) {
    stop(paste("No files provided for", context), call. = FALSE)
  }

  missing <- file_paths[!file.exists(file_paths)]

  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing files for ", context, ":\n",
        paste("  -", basename(missing), collapse = "\n")
      ),
      call. = FALSE
    )
  }
  file_paths
}


#' Validate filter result is not empty
#'
#' Specialized validation for filter operations. Provides detailed context
#' about what filter was applied and what values are available.
#'
#' @param df The filtered dataframe to check.
#' @param filter_col The name of the column that was filtered.
#' @param filter_val The value(s) that were used in the filter.
#' @param original_df Optional original dataframe to show available values.
#' @param context A string describing the operation context.
#' @return The input dataframe if not empty.
#' @examples
#' filtered <- df %>% filter(CANCER.TYPE == "Breast")
#' validate_filter_result(filtered, "CANCER.TYPE", "Breast", df, "FDS data")
#' @export
validate_filter_result <- function(df, filter_col, filter_val, original_df = NULL, context = "") {
  if (is.null(df) || nrow(df) == 0) {
    msg <- paste0(
      "Filter returned no results",
      if (context != "") paste0(" in ", context) else "",
      "\nFilter: ", filter_col, " == ",
      if (length(filter_val) > 1) {
        paste0("[", paste(filter_val, collapse = ", "), "]")
      } else {
        paste0("'", filter_val, "'")
      }
    )

    if (!is.null(original_df) && filter_col %in% names(original_df)) {
      available <- unique(original_df[[filter_col]])
      available <- available[!is.na(available)]
      if (length(available) <= 20) {
        msg <- paste0(msg, "\nAvailable values: ", paste(available, collapse = ", "))
      } else {
        msg <- paste0(msg, "\nAvailable values (first 20): ", paste(head(available, 20), collapse = ", "), ", ...")
      }
    }

    stop(msg, call. = FALSE)
  }
  df
}


#' Safely execute a function with error context
#'
#' Wraps a function call in error handling that adds context to any errors.
#' This helps trace where in the pipeline an error originated.
#'
#' @param expr The expression to evaluate.
#' @param context A string describing the operation being performed.
#' @param on_error What to do on error: "stop" (default), "warn", or "null".
#' @return The result of the expression, or NULL if on_error is "null" or "warn".
#' @examples
#' result <- safe_execute(process_fds_raw(data, config), "FDS raw processing")
#' @export
safe_execute <- function(expr, context, on_error = "stop") {
  tryCatch(
    expr,
    error = function(e) {
      full_msg <- paste0("Error in ", context, ": ", e$message)

      if (on_error == "stop") {
        stop(full_msg, call. = FALSE)
      } else if (on_error == "warn") {
        warning(full_msg, call. = FALSE)
        return(NULL)
      } else {
        return(NULL)
      }
    }
  )
}


#' Assert a condition with a custom error message
#'
#' Simple assertion function for validating conditions.
#'
#' @param condition A logical condition to check.
#' @param message Error message if the condition is FALSE.
#' @return TRUE invisibly if the condition passes.
#' @examples
#' assert(nrow(df) > 0, "Dataframe must not be empty")
#' @export
assert <- function(condition, message) {
  if (!condition) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}
