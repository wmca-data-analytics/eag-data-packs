# Read Tumour Data - Master Function
# ===================================
# This file provides the main entry point for loading tumour-specific data.
# It orchestrates all the processing functions to produce a standardised
# named list structure for each tumour type.
#
# Usage:
#   source("R/load_tumour_config.R")
#   source("R/tumour_processing.R")
#   source("R/read_tumour_data.R")
#
#   lung_data <- read_tumour_data("lung", global_data)
#   lung_data$fds$summary
#   lung_data$day_62$plot$compliance


#' Apply CWT provider trust exclusions to global data
#'
#' Filters out specified trust codes from CWT provider-level dataframes in
#' global_data based on the tumour config's cwt_provider_exclusions field.
#' This allows certain trusts to be excluded from CWT data for specific
#' tumour types (e.g. Head & Neck excludes RL1, RRJ, RLT).
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration list (from get_tumour_config)
#' @return Modified global_data with excluded trusts filtered out
apply_cwt_provider_exclusions <- function(global_data, config) {
  exclusions <- config$cwt_provider_exclusions

  if (is.null(exclusions) || length(exclusions) == 0) {
    return(global_data)
  }

  message(paste0("  Applying CWT provider exclusions for ", config$name,
                 ": removing trusts ", paste(exclusions, collapse = ", ")))

  # Mapping of global_data element names to their provider code columns
  cwt_provider_columns <- list(
    fds_df           = "ORG.CODE",
    day_31_df        = "ORG.CODE",
    day_62_df        = "ORG.CODE",
    latest_public_2_months_df = "ORG.CODE",
    latest_cwt_d_df  = "Provider.Code.FDS",
    cwt_d_fds_df     = "Provider.Code.FDS",
    cwt_d_31_day_df  = "ProviderCode",
    cwt_d_62_day_df  = "AccountableProvider"
  )

  for (df_name in names(cwt_provider_columns)) {
    col_name <- cwt_provider_columns[[df_name]]

    if (!is.null(global_data[[df_name]]) && col_name %in% names(global_data[[df_name]])) {
      rows_before <- nrow(global_data[[df_name]])
      global_data[[df_name]] <- global_data[[df_name]][
        !global_data[[df_name]][[col_name]] %in% exclusions, , drop = FALSE
      ]
      rows_removed <- rows_before - nrow(global_data[[df_name]])
      if (rows_removed > 0) {
        message(paste0("    ", df_name, ": removed ", rows_removed,
                       " rows (column: ", col_name, ")"))
      }
    }
  }

  global_data
}


#' Read and process all data for a tumour type
#'
#' This is the main entry point for loading tumour-specific data. It returns
#' a nested list containing all processed dataframes for the specified tumour.
#'
#' @param tumour_key Character string identifying the tumour type (e.g., "lung", "breast")
#' @param global_data List containing all global dataframes needed for processing.
#'   Expected elements:
#'   - fds_df: FDS provider data
#'   - cwt_fds_comm_df: FDS commissioner data
#'   - cwt_d_fds_df: Detailed FDS provider data
#'   - cwt_d_fds_comm_df: Detailed FDS commissioner data
#'   - day_31_df: 31-day provider data
#'   - cwt_d_31_day_df: Detailed 31-day data
#'   - day_62_df: 62-day provider data
#'   - cwt_d_62_day_df: Detailed 62-day data
#'   - cwt_62d_comm_df: 62-day commissioner data
#'   - stage_completeness_df: Stage completeness data
#'   - stage_df: Staging data
#'   - latest_public_2_months_df: Benchmarking data
#'   - latest_cwt_d_df: Latest detailed CWT data
#'   - tumour_pack_lwbc_plot_df: LWBC plot data
#'   - tumour_pack_trust_lwbc_plot_df: LWBC trust plot data
#'   - tumour_pack_alliance_trust_lwbc_table_df: LWBC table data
#'   - tv_long: Long-format treatment variation data
#'   - tv_indicators: Indicator strings + chosen display period per tumour
#'
#' @return Named list with the following structure:
#' \describe{
#'   \item{fds}{FDS data (raw, summary, rule_in, rule_in_comm, plot, table, correlation)}
#'   \item{day_31}{31-day data (raw, plot, table)}
#'   \item{day_62}{62-day data (raw, summary, plot, table, correlation)}
#'   \item{staging}{Staging data (completeness, early_diagnosis)}
#'   \item{sub_types}{Sub-type staging data (only for tumours with sub-types)}
#'   \item{benchmarking}{Benchmark tables (day_28, day_31, day_62, fds_end_reason)}
#'   \item{correlation}{FDS vs 62-day correlation data}
#'   \item{standards_table}{Combined FDS/31-day/62-day standards table}
#'   \item{lwbc}{LWBC data (plot, trust_plot, table)}
#'   \item{treatment_variation}{Treatment variation data (overview, tumour)}
#'   \item{counts}{Various count values for trust above/below targets}
#'   \item{dates}{Latest dates for each standard}
#' }
#'
#' @examples
#' \dontrun{
#' # Load required functions
#' source("R/load_tumour_config.R")
#' source("R/tumour_processing.R")
#' source("R/read_tumour_data.R")
#'
#' # Read lung data
#' lung_data <- read_tumour_data("lung", global_data)
#'
#' # Access specific data
#' lung_data$fds$summary$COMPLIANCE
#' lung_data$day_62$plot$compliance
#' lung_data$staging$early_diagnosis$gender
#' }
#'
#' @export
read_tumour_data <- function(tumour_key, global_data) {

  # Get and validate configuration
  config <- get_tumour_config(tumour_key)
  validate_tumour_config(tumour_key)

  # Apply CWT provider trust exclusions (if any for this tumour type)
  global_data <- apply_cwt_provider_exclusions(global_data, config)

  message(paste("Processing tumour data for:", config$name))

  # Initialise result structure
  result <- list(
    config = config,
    fds = list(),
    day_31 = list(),
    day_62 = list(),
    staging = list(completeness_subtypes = NULL),
    sub_types = NULL,
    benchmarking = list(),
    correlation = list(),
    standards_table = NULL,
    lwbc = list(),
    treatment_variation = NULL,
    counts = list(),
    dates = list()
  )

  # =========================================================================
  # FDS PROCESSING
  # =========================================================================
  message("  Processing FDS data...")

  fds_raw <- process_fds_raw(global_data, config)
  result$fds$raw <- fds_raw

  result$fds$summary <- process_fds_summary(fds_raw, config)

  result$fds$rule_in <- process_fds_rule_in(fds_raw)
  result$fds$rule_in_comm <- process_fds_rule_in_comm(fds_raw)

  result$fds$plot <- process_fds_plot(fds_raw, config)

  fds_table_result <- process_fds_table(result$fds$plot, config)
  result$fds$table <- fds_table_result$table
  result$dates$fds <- fds_table_result$latest_date

  # Prepend provider group names to chart labels (after table data extracted)
  result$fds$plot <- add_group_labels_to_plot_list(result$fds$plot, config)

  result$fds$correlation <- process_fds_correlation(fds_raw, result$fds$plot)

  # FDS counts
  result$counts$fds_trusts_above <- result$fds$plot$count_above
  result$counts$fds_trusts_below <- result$fds$plot$count_below
  result$counts$fds_ri_trusts_above <- result$fds$rule_in$count_above
  result$counts$fds_ri_trusts_below <- result$fds$rule_in$count_below
  result$counts$fds_ri_trusts_above_comm <- result$fds$rule_in_comm$count_above
  result$counts$fds_ri_trusts_below_comm <- result$fds$rule_in_comm$count_below

  # =========================================================================
  # 31-DAY PROCESSING
  # =========================================================================
  message("  Processing 31-day data...")

  raw_31 <- process_31_day_raw(global_data, config)
  result$day_31$raw <- raw_31

  result$day_31$plot <- process_31_day_plot(raw_31, config)

  table_31_result <- process_31_day_table(result$day_31$plot, config)
  result$day_31$table <- table_31_result$table
  result$dates$day_31 <- table_31_result$latest_date

  # Prepend provider group names to chart labels (after table data extracted)
  result$day_31$plot <- add_group_labels_to_plot_list(result$day_31$plot, config)

  # =========================================================================
  # 62-DAY PROCESSING
  # =========================================================================
  message("  Processing 62-day data...")

  raw_62 <- process_62_day_raw(global_data, config)
  result$day_62$raw <- raw_62

  result$day_62$summary <- process_62_day_summary(raw_62, config)

  result$day_62$plot <- process_62_day_plot(raw_62, config)

  table_62_result <- process_62_day_table(result$day_62$plot, config)
  result$day_62$table <- table_62_result$table
  result$dates$day_62 <- table_62_result$latest_date

  # Prepend provider group names to chart labels (after table data extracted)
  result$day_62$plot <- add_group_labels_to_plot_list(result$day_62$plot, config)

  result$day_62$correlation <- process_62_day_correlation(raw_62, config)

  # 62-day counts
  if (!is.null(result$day_62$plot)) {
    result$counts$day_62_trusts_above <- result$day_62$plot$count_above
    result$counts$day_62_trusts_below <- result$day_62$plot$count_below
  }

  # =========================================================================
  # STAGING PROCESSING
  # =========================================================================
  message("  Processing staging data...")

  # Staging completeness
  if (!is.null(config$staging_completeness)) {
    if (has_split(config, "staging_completeness")) {
      result$staging$completeness_subtypes <- process_staging_completeness_subtypes(global_data, config)
      # Set date from first available group
      first <- result$staging$completeness_subtypes[[1]]
      if (!is.null(first)) result$dates$staging <- first$current_quarter
    } else {
      sc_raw <- process_staging_completeness_raw(global_data, config)
      result$staging$completeness <- process_staging_completeness_plot(sc_raw)
      if (!is.null(result$staging$completeness)) {
        result$dates$staging <- result$staging$completeness$current_quarter
      }
    }
  }

  # Staging demographics
  if (!is.null(config$staging_demographics)) {
    if (has_split(config, "staging_demographics")) {
      # Process split staging
      result$sub_types <- process_staging_with_subtypes(global_data, config)
    } else {
      # Process standard staging
      stage_raw <- process_staging_raw(global_data, config)
      result$staging$early_diagnosis <- process_staging_demographics(stage_raw, config)
    }
  }

  # =========================================================================
  # BENCHMARKING PROCESSING
  # =========================================================================
  message("  Processing benchmarking data...")

  result$benchmarking <- process_benchmarking(global_data, config)

  # =========================================================================
  # CORRELATION PROCESSING
  # =========================================================================
  message("  Processing correlation data...")

  result$correlation <- process_fds_62_correlation(
    result$fds$correlation,
    result$day_62$correlation
  )

  # =========================================================================
  # STANDARDS TABLE
  # =========================================================================
  message("  Creating standards table...")

  result$standards_table <- create_standards_table(
    result$fds$table,
    result$day_31$table,
    result$day_62$table,
    config
  )

  # =========================================================================
  # LWBC PROCESSING
  # =========================================================================
  message("  Processing LWBC data...")

  result$lwbc <- process_lwbc(global_data, config)

  # =========================================================================
  # TREATMENT VARIATION PROCESSING
  # =========================================================================
  if (!is.null(config$treatment_variation)) {
    message("  Processing treatment variation data...")
    result$treatment_variation <- process_treatment_variation(global_data, config)
  }

  message(paste("  Completed processing for:", config$name))

  result
}


#' Create global data list from environment
#'
#' Helper function to create the global_data list from dataframes
#' that exist in the current environment. This is useful when transitioning
#' from the old script-based approach.
#'
#' @param env Environment to search for dataframes (default: global environment)
#' @return List containing all required global dataframes
#'
#' @examples
#' \dontrun{
#' # After running existing data loading scripts
#' global_data <- create_global_data_list()
#' lung_data <- read_tumour_data("lung", global_data)
#' }
#'
#' @export
create_global_data_list <- function(env = .GlobalEnv) {
  required_vars <- c(
    "fds_df",
    "cwt_fds_comm_df",
    "cwt_d_fds_df",
    "cwt_d_fds_comm_df",
    "day_31_df",
    "cwt_d_31_day_df",
    "day_62_df",
    "cwt_d_62_day_df",
    "cwt_d_62_day_comm_df",
    "cwt_62d_comm_df",
    "stage_completeness_df",
    "stage_df",
    "latest_public_2_months_df",
    "latest_cwt_d_df",
    "tumour_pack_lwbc_plot_df",
    "tumour_pack_lwbc_pa_plot_df",
    "tumour_pack_trust_lwbc_plot_df",
    "tumour_pack_trust_lwbc_pa_plot_df",
    "tumour_pack_alliance_trust_lwbc_table_df"
  )

  # Optional treatment variation data
  tv_vars <- c("tv_long", "tv_indicators")

  result <- list()

  for (var in required_vars) {
    if (exists(var, envir = env)) {
      result[[var]] <- get(var, envir = env)
    } else {
      warning(paste("Missing required variable:", var))
      result[[var]] <- NULL
    }
  }

  for (var in tv_vars) {
    if (exists(var, envir = env)) {
      result[[var]] <- get(var, envir = env)
    }
  }

  result
}


#' Read all tumour data
#'
#' Convenience function to read data for all configured tumour types.
#'
#' @param global_data List containing all global dataframes
#' @param tumour_keys Character vector of tumour keys to process.
#'   Default is all configured tumours.
#' @return Named list of tumour data, keyed by tumour_key
#'
#' @examples
#' \dontrun{
#' all_data <- read_all_tumour_data(global_data)
#' all_data$lung$fds$summary
#' all_data$breast$day_62$plot
#' }
#'
#' @export
read_all_tumour_data <- function(global_data, tumour_keys = NULL) {
  if (is.null(tumour_keys)) {
    tumour_keys <- get_tumour_keys()
  }

  result <- list()

  for (key in tumour_keys) {
    tryCatch({
      result[[key]] <- read_tumour_data(key, global_data)
    }, error = function(e) {
      warning(paste("Failed to process", key, ":", e$message))
      result[[key]] <- NULL
    })
  }

  result
}


#' Get data path mapping for RMarkdown migration
#'
#' Returns a mapping of old dataframe names to new named list paths.
#' Useful for updating RMarkdown files during migration.
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Data frame with columns: old_name, new_path, description
#'
#' @examples
#' \dontrun{
#' mapping <- get_data_path_mapping("lung")
#' # Use to find/replace in RMarkdown files
#' }
#'
#' @export
get_data_path_mapping <- function(tumour_key) {
  # This provides a reference for updating RMarkdown files
  data.frame(
    old_name = c(
      # FDS
      "wmca_tumour_fds_df",
      "wmca_tumour_fds_comm_df",
      "wmca_tumour_d_fds_df",
      "wmca_tumour_d_fds_comm_df",
      "wmca_tumour_fds_summary_df",
      "wmca_tumour_ri_fds_df",
      "wmca_tumour_ri_fds_summary_df",
      "wmca_tumour_ri_trust_fds_df",
      "wmca_tumour_ri_trusts_above_fds_df",
      "wmca_tumour_ri_trusts_below_fds_df",
      "number_tumour_ri_trusts_above_fds",
      "number_tumour_ri_trusts_below_fds",
      "wmca_tumour_ri_fds_comm_df",
      "wmca_tumour_ri_fds_summary_comm_df",
      "wmca_tumour_ri_trust_fds_comm_df",
      "wmca_tumour_ri_trusts_above_fds_comm_df",
      "wmca_tumour_ri_trusts_below_fds_comm_df",
      "number_tumour_ri_trusts_above_fds_comm_df",
      "number_tumour_ri_trusts_below_fds_comm_df",
      "wmca_tumour_total_treated_plot_df",
      "wmca_tumour_fds_plot_df",
      "tumour_trusts_above_fds_df",
      "tumour_trusts_below_fds_df",
      "number_tumour_trusts_above_fds",
      "number_tumour_trusts_below_fds",
      "wmca_tumour_riro_plot_df",
      "wmca_tumour_riro_bar_plot_df",
      "wmca_tumour_fds_table_df",
      "latest_fds_date",
      "wmca_tumour_fds_correlation_df",
      "wmca_tumour_fds_ro_correlation_df",
      "wmca_tumour_fds_ri_correlation_df",
      # 31-day
      "wmca_tumour_31_df",
      "wmca_tumour_d_31_df",
      "wmca_tumour_31_plot_df",
      "wmca_tumour_31_table_df",
      "latest_31_day_date",
      # 62-day
      "wmca_tumour_62_df",
      "wmca_tumour_d_62_df",
      "wmca_tumour_62_comm_df",
      "wmca_tumour_62_summary_df",
      "wmca_tumour_total_treated_62_plot_df",
      "wmca_tumour_62_plot_df",
      "tumour_trusts_above_62_day_df",
      "tumour_trusts_below_62_day_df",
      "number_tumour_trusts_above_62_day",
      "number_tumour_trusts_below_62_day",
      "wmca_tumour_62_table_df",
      "latest_62_day_date",
      "wmca_tumour_62_correlation_df",
      # Staging
      "wmca_tumour_sc_df",
      "wmca_tumour_sc_plot_df",
      "wmca_tumour_sc_fig_df",
      "current_diagnosis_quarter",
      "wmca_tumour_stage_df",
      "wmca_tumour_stage_plot_df",
      "wmca_tumour_trust_stage_plot_df",
      "wmca_tumour_gender_stage_plot_df",
      "wmca_tumour_age_stage_plot_df",
      "wmca_tumour_ethnicity_stage_plot_df",
      "wmca_tumour_deprivation_stage_plot_df",
      "wmca_tumour_route_stage_plot_df",
      # Benchmarking
      "wmca_tumour_standards_table_df",
      "standard_28_day_tumour_table_df",
      "standard_31_day_tumour_table_df",
      "standard_62_day_tumour_table_df",
      "standard_fds_end_reason_df",
      # Correlation
      "wmca_tumour_fds_vs_62_df",
      "wmca_tumour_fds_ro_vs_62_df",
      "wmca_tumour_fds_ri_vs_62_df",
      # LWBC
      "wmca_tumour_lwbc_plot_df",
      "wmca_tumour_lwbc_pa_plot_df",
      "wmca_tumour_trust_lwbc_plot_df",
      "wmca_tumour_trust_lwbc_pa_plot_df",
      "wmca_tumour_lwbc_table_df",
      # Treatment variation
      "tumour_overview_slide",
      "tumour_slide"
    ),
    new_path = c(
      # FDS
      "tumour_data$fds$raw$provider",
      "tumour_data$fds$raw$provider_comm",
      "tumour_data$fds$raw$detailed",
      "tumour_data$fds$raw$detailed_comm",
      "tumour_data$fds$summary$provider",
      "tumour_data$fds$rule_in$raw",
      "tumour_data$fds$rule_in$summary",
      "tumour_data$fds$rule_in$trust",
      "tumour_data$fds$rule_in$trusts_above",
      "tumour_data$fds$rule_in$trusts_below",
      "tumour_data$counts$fds_ri_trusts_above",
      "tumour_data$counts$fds_ri_trusts_below",
      "tumour_data$fds$rule_in_comm$raw",
      "tumour_data$fds$rule_in_comm$summary",
      "tumour_data$fds$rule_in_comm$trust",
      "tumour_data$fds$rule_in_comm$trusts_above",
      "tumour_data$fds$rule_in_comm$trusts_below",
      "tumour_data$counts$fds_ri_trusts_above_comm",
      "tumour_data$counts$fds_ri_trusts_below_comm",
      "tumour_data$fds$plot$total_treated",
      "tumour_data$fds$plot$compliance",
      "tumour_data$fds$plot$trusts_above",
      "tumour_data$fds$plot$trusts_below",
      "tumour_data$counts$fds_trusts_above",
      "tumour_data$counts$fds_trusts_below",
      "tumour_data$fds$plot$riro",
      "tumour_data$fds$plot$riro_bar",
      "tumour_data$fds$table",
      "tumour_data$dates$fds",
      "tumour_data$fds$correlation$overall",
      "tumour_data$fds$correlation$rule_out",
      "tumour_data$fds$correlation$rule_in",
      # 31-day
      "tumour_data$day_31$raw$public",
      "tumour_data$day_31$raw$detailed",
      "tumour_data$day_31$plot$plot",
      "tumour_data$day_31$table",
      "tumour_data$dates$day_31",
      # 62-day
      "tumour_data$day_62$raw$public",
      "tumour_data$day_62$raw$detailed",
      "tumour_data$day_62$raw$comm",
      "tumour_data$day_62$summary$provider",
      "tumour_data$day_62$plot$total_treated",
      "tumour_data$day_62$plot$compliance",
      "tumour_data$day_62$plot$trusts_above",
      "tumour_data$day_62$plot$trusts_below",
      "tumour_data$counts$day_62_trusts_above",
      "tumour_data$counts$day_62_trusts_below",
      "tumour_data$day_62$table",
      "tumour_data$dates$day_62",
      "tumour_data$day_62$correlation",
      # Staging
      "(staging completeness raw - internal)",
      "tumour_data$staging$completeness$plot",
      "tumour_data$staging$completeness$figure",
      "tumour_data$dates$staging",
      "(staging raw - internal)",
      "tumour_data$staging$early_diagnosis$overall",
      "tumour_data$staging$early_diagnosis$trust",
      "tumour_data$staging$early_diagnosis$gender",
      "tumour_data$staging$early_diagnosis$age",
      "tumour_data$staging$early_diagnosis$ethnicity",
      "tumour_data$staging$early_diagnosis$deprivation",
      "tumour_data$staging$early_diagnosis$route",
      # Benchmarking
      "tumour_data$standards_table",
      "tumour_data$benchmarking$day_28",
      "tumour_data$benchmarking$day_31",
      "tumour_data$benchmarking$day_62",
      "tumour_data$benchmarking$fds_end_reason",
      # Correlation
      "tumour_data$correlation$fds_vs_62",
      "tumour_data$correlation$fds_ro_vs_62",
      "tumour_data$correlation$fds_ri_vs_62",
      # LWBC
      "tumour_data$lwbc$plot",
      "tumour_data$lwbc$pa_plot",
      "tumour_data$lwbc$trust_plot",
      "tumour_data$lwbc$trust_pa_plot",
      "tumour_data$lwbc$table",
      # Treatment variation
      "tumour_data$treatment_variation$overview",
      "tumour_data$treatment_variation$tumour"
    ),
    stringsAsFactors = FALSE
  )
}
