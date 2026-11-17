# ==============================================================================
# Alliance Configuration Accessors
# ==============================================================================
# These functions provide access to alliance-specific configuration stored in
# .alliance_configs (loaded by R/load_alliance_config.R). They allow scripts to retrieve
# trust codes, lookups, and other alliance-specific data without hardcoding
# if/else blocks throughout the codebase.

#' Get the current alliance code
#'
#' Returns the alliance code from the global environment, with validation.
#'
#' @param alliance Optional. Override the global alliance variable.
#' @return Character string: "WMCA" or "EMCA"
get_current_alliance <- function(alliance = NULL) {
  if (is.null(alliance)) {
    if (!exists("alliance", envir = .GlobalEnv)) {
      stop("Global 'alliance' variable not set. Set it in main.R or pass explicitly.")
    }
    alliance <- get("alliance", envir = .GlobalEnv)
  }
  toupper(alliance)
}

#' Get alliance configuration
#'
#' Returns the full configuration list for an alliance, or a specific element.
#'
#' @param key Optional. Specific configuration key to retrieve.
#' @param alliance Optional. Override the global alliance variable.
#' @return Configuration list or specific element.
get_alliance_config <- function(key = NULL, alliance = NULL) {
  alliance <- get_current_alliance(alliance)
  config <- .alliance_configs[[alliance]]

  if (is.null(config)) {
    stop(paste0("Unknown alliance: '", alliance, "'. Valid options: ",
                paste(names(.alliance_configs), collapse = ", ")))
  }

  if (is.null(key)) {
    return(config)
  }

  if (!key %in% names(config)) {
    stop(paste0("Unknown config key: '", key, "'. Valid keys: ",
                paste(names(config), collapse = ", ")))
  }

  config[[key]]
}

#' Get organisation recode map (named vector for dplyr::recode)
#'
#' Derived from org_lookup so there is a single source of truth for
#' trust code to name mappings.
#'
#' @param alliance Optional. Override the global alliance variable.
#' @return Named character vector (code -> display name).
get_org_recode_map <- function(alliance = NULL) {
  lookup <- get_alliance_config("org_lookup", alliance)
  setNames(lookup$ORG.NAME, lookup$ORG.CODE)
}

#' Get trust to ICB mapping
#'
#' The raw map stores org codes -> ICB names. This function resolves
#' the codes to display names via org_lookup, so the returned vector
#' is keyed by display name rather than code.
#'
#' @param alliance Optional. Override the global alliance variable.
#' @return Named character vector (trust display name -> ICB name).
get_trust_to_icb <- function(alliance = NULL) {
  map <- get_alliance_config("trust_to_icb", alliance)
  lookup <- get_alliance_config("org_lookup", alliance)
  name_lookup <- setNames(lookup$ORG.NAME, lookup$ORG.CODE)
  setNames(map, name_lookup[names(map)])
}

#' Get source name map (source trust name -> display name)
#'
#' Generic function for any map that stores source trust names -> org codes.
#' Resolves the codes to canonical display names via org_lookup, so there
#' is a single source of truth for trust names regardless of data source.
#'
#' @param map_name Character. The config key for the map, e.g.
#'   "stage_geography_map", "lwbc_geography_map".
#' @param alliance Optional. Override the global alliance variable.
#' @return Named character vector (source trust name -> display name).
get_source_name_map <- function(map_name, alliance = NULL) {
  map <- get_alliance_config(map_name, alliance)
  lookup <- get_alliance_config("org_lookup", alliance)
  name_lookup <- setNames(lookup$ORG.NAME, lookup$ORG.CODE)
  setNames(name_lookup[map], names(map))
}

#' Get treatment variation configuration
#' @param key Optional. Specific TV config key.
#' @param alliance Optional. Override the global alliance variable.
#' @return Treatment variation config list or specific element.
get_tv_config <- function(key = NULL, alliance = NULL) {
  tv_config <- get_alliance_config("tv_config", alliance)

  if (is.null(key)) {
    return(tv_config)
  }

  if (!key %in% names(tv_config)) {
    stop(paste0("Unknown TV config key: '", key, "'"))
  }

  tv_config[[key]]
}

#' Check if current alliance needs percentage conversion for TV data
#' @param alliance Optional. Override the global alliance variable.
#' @return Logical.
needs_tv_percentage_conversion <- function(alliance = NULL) {
  get_tv_config("needs_percentage_conversion", alliance)
}


# ==============================================================================
# CWT Calculation Functions
# ==============================================================================

#' Calculate CWT compliance figure
#'
#' Calculate a CWT compliance figure, *i.e.* no. of patients **within a standard**
#' divided by no. of patients **referred or treated** (depending on standard).
#' Note that "within a standard" can mean several things:
#' - FDS: receiving a diagnosis or ruling out of cancer within 28 days.
#' - 31-day: receiving treatment within 31 days of decision to treat (DTT).
#' - 62-day: receiving treatment within 62 days of referral.
#'
#' Note we use "ifelse" because it can handle vectors; a simple "if" cannot.
#'
# TODO: Catch if arguments are not integers and don't end in 0.5, e.g. 0.3
#       This can be done with existing function `num_is_whole_half`
#       Easy enough to catch, BUT what do we do if it's NOT a whole or half?
#'
#' Note that the 62-day standard deals with half-patients, so we cannot just
#' work with integers here.
#'
#' If denominator is zero, return NA instead of trying to divide by zero
#' (which will fail).
#'
#' @param within_standard No. of patients within the standard.
#' @param total_patients Total no. of patients in the cohort.
#' @return Compliance as a percentage, to 3 decimal places (*e.g.* 73).
#' @examples
#' calc_cwt_compliance(8, 10)
#' calc_cwt_compliance(22.5, 142.5)
#' @export
calc_cwt_compliance <- function(within_standard, total_patients) {
  ifelse(total_patients != 0,
    round((within_standard / total_patients) * 100, digits = 1),
    NA
  )
}


#' Calculate Staging Completeness figure
#'
#' Calculate a staging completeness figure, **i.e.** no. of patients given a stage
#' at an MDT meeting, out of the entire cohort discussed at an MDT meeting.
#'
#' If denominator is zero, do something instead of trying to divide by zero
#' (which will fail).
#'
#' @param mdt_stage No. of patients given a stage at MDT.
#' @param mdt_discussed No. of patients discussed at MDT.
#' @return Staging completeness as a percentage, to 3 decimal places (*e.g.* 73).
#' @examples
#' calc_staging_completeness(8, 10)
#' @export
calc_staging_completeness <- function(mdt_stage, mdt_discussed) {
  # `ifelse` can handle vectors; a simple `if` cannot.
  # TODO: Are we sure returning zero is the right thing? Returning NaN (because
  #       are trying to divide by zero) messes up refactoring, for example.
  completeness <- ifelse(mdt_discussed != 0,
    (mdt_stage / mdt_discussed) * 100,
    0
  )
  round(completeness, digits = 3)
}


#' Confirm a number is either whole, or a half
#'
#' Check whether a number is either whole (an integer number, but note this is
#' different to being an R integer data type), or a half (e.g. 3.5). Do this by
#' looking at the fractional part.
#'
#' @param num_to_check Numeric. The number to check.
#' @return Logical. TRUE if a whole number or a half; FALSE otherwise.
#' @examples
#' num_is_whole_half(5)
#' # Returns TRUE
#' num_is_whole_half(3.5)
#' # Returns TRUE
#' num_is_whole_half(2.3)
#' # Returns FALSE
#' @export
num_is_whole_half <- function(num_to_check) {
  
  ifelse(num_to_check %% 1 == 0 | num_to_check %% 1 == 0.5, TRUE, FALSE)

}

#' Round a number up to the nearest half
#' 
#' Rounds a number up to the nearest half. Does this by multiplying the number times
#' 2, rounding **up** to the nearest integer, then dividing by 2.
#' 
#' @param x The number to round.
#' @return The rounded number
#' @examples
#' ceiling_to_nearest_half(1.2)
#' ceiling_to_nearest_half(16.8)
#' ceiling_to_nearest_half(72)
#' @export

ceiling_to_nearest_half <- function(x) {
  ceiling(x * 2) / 2
}

#' Gets 62 day target as patients
#' 
#' Get the number of the patients needed to be treated to reach the 62 day standard
#' target of 75% of patients receiving treatment within 62 days of referral. This is
#' done by first calculating the target number of patients and if that value is not
#' a half or a whole (e.g. 2 or 2.5) then using then ceiling_to_nearest_half function 
#' to round the value up to the nearest whole or half number.
#' 
#' @param patients Total no. of patients in the cohort.
#' @return Number of patients required to be treated as an integer or to the nearest
#' 0.5.
#' @examples
#' get_62_day_target_as_patients(22)
#' get_62_day_target_as_patients(65)
#' get_62_day_target_as_patients(103)
#' @export

get_62_day_target_as_patients <- function(patients) {
  target <- patients * CWT_STANDARD_62DAY
  
  ifelse(num_is_whole_half(target), target, ceiling_to_nearest_half(target))
  
}

#' Get CWT standard targets as patients
#'
#' Get the number of patients that needed to be referred or treated to reach the
#' standards target *i.e.* total number of patients **referred or treated**
#' (depending on standard) multiplied by the **target percent of patients** for
#' that standard (as a decimal).
#' Note that "target percentage of patients" can mean several things:
#' - fds : 80% of patients receiving a diagnosis or ruling out of cancer within 28 days.
#' - 31 day: 96% of patients receiving treatment within 31 days of decision to treat (DTT).
#' - 62 day: 75% of patients receiving treatment within 62 days of referral.
#'
#' @param standard The standard as a string from the following options:
#' - 'fds'
#' - '31 day'
#' - '62 day'
#' @param patients Total no. of patients in the cohort.
#' @return Number of patients required to be referred or treated as an integer or
#' to the nearest 0.5 for **62 day only**.
#' @examples
#' get_cwt_targets_as_patients("fds", 53)
#' get_cwt_targets_as_patients("31 day", 123)
#' get_cwt_targets_as_patients("62 day", 78)
#' @export

get_cwt_targets_as_patients <- function(standard, patients) {
  case_when(
    patients == 0 ~ NA_real_,
    standard == "fds" || standard == "FDS" || standard == "28-day FDS"  ~ ceiling(patients * CWT_STANDARD_FDS),
    standard == "31 day" || standard == "31-day" ||standard == "31D" || standard == "31-day Combined" ~ ceiling(patients * CWT_STANDARD_31DAY),
    standard == "62 day" || standard == "62-day" ||standard == "62D" || standard == "62-day Combined" ~ get_62_day_target_as_patients(patients)
  )
}

#' Change number to 'X above' or 'X below'
#'
#' Append text to a number so that it can be used to create friendly output,
#' primarily in relation to difference in number of patients between actual
#' performance and a CWT standard.
#' Any number which is above zero will be classed as having exceeded the standard,
#' while a number below zero will be classed as being short of the standard.
#'
#' @param number The number to process
#' @return The friendly description of the number in relation to the standard,
#' as a string.
#' @examples
#' get_pt_diff_friendly(-3)
#' get_pt_diff_friendly(54)
#' get_pt_diff_friendly(0)

get_pt_diff_friendly <- function(number) {
  case_when(
    number < 0 ~ paste0(abs(number), " below"),
    number > 0 ~ paste0(number, " above"),
    number == 0 ~ "met std"
  )
}


#' Round a number to the nearest 0.5
#'
#' Rounds a number to the nearest half. Does this by multiplying by 2,
#' rounding to the nearest integer, then dividing by 2. Examples:
#' 1.2 * 2 = 2.4 -> round(2.4) = 2 -> 2 / 2 = 1.0
#' 1.6 * 2 = 3.2 -> round(3.2) = 3 -> 3 / 2 = 1.5
#' 2.7 * 2 = 5.4 -> round(5.4) = 5 -> 5 / 2 = 2.5
#' 3.1 * 2 = 6.2 -> round(6.2) = 6 -> 6 / 2 = 3.0
#' 3.5 * 2 = 7.0 -> round(7.0) = 7 -> 7 / 2 = 3.5
#' 3.9 * 2 = 7.8 -> round(7.8) = 8 -> 8 / 2 = 4.0
#'
#' @param x Double. The number to round.
#' @return Double. The rounded number.
#' @examples
#' round_to_nearest_half(3.75)
#' round_to_nearest_half(2.354)
round_to_nearest_half <- function(x) {
  round(x * 2) / 2
}


#' Parse financial year date string to Date object
#'
#' Converts a financial year date string in the format "MMM YYYY/YY"
#' (e.g., "APR 2024/25", "FEB 2024/25") to a proper R Date object.
#' Correctly handles months that fall in the start year (Apr-Dec) versus
#' those that fall in the end year (Jan-Mar).
#'
#' @param date_str Character. A date string in financial year format,
#'   e.g., "APR 2024/25" or "FEB 2024/25".
#' @return A Date object representing the first day of the specified month.
#' @examples
#' parse_financial_year_date("APR 2024/25")
#' # Returns 2024-04-01
#' parse_financial_year_date("FEB 2024/25")
#' # Returns 2025-02-01
#' @export
parse_financial_year_date <- function(date_str) {
  # Extract month abbreviation (e.g., "APR", "FEB")
  month_abbr <- sub(" (.*)", "", date_str)

  # Extract the full start year (e.g., "2024")
  year_start_match <- sub(".*([0-9]{4})/.*", "\\1", date_str)
  year_start <- as.integer(year_start_match)

  # Extract the two-digit end year (e.g., "25")
  year_end_short_match <- sub(".*/([0-9]{2})$", "\\1", date_str)
  year_end_short <- as.integer(year_end_short_match)

  # Determine full end year (e.g., 25 -> 2025).
  # Assuming 2-digit years '00'-'49' refer to 20xx, and '50'-'99' refer to 19xx for safety.
  # This makes '25' become 2025.
  year_end_full <- ifelse(year_end_short < 50, 2000 + year_end_short, 1900 + year_end_short)

  # Determine the calendar year based on the month
  # Months January (1) to March (3) typically belong to the *end* calendar year of the financial year
  # Months April (4) to December (12) typically belong to the *start* calendar year of the financial year
  # We need to convert month_abbr to a numeric month to compare.
  month_num <- match(toupper(month_abbr), toupper(month.abb)) # month.abb is built-in R abbr month names

  # Initialize final_year for vectorization
  final_year <- integer(length(date_str))

  for (i in seq_along(date_str)) {
    if (month_num[i] >= 4) { # April (4) to December (12)
      final_year[i] <- year_start[i]
    } else { # January (1) to March (3)
      final_year[i] <- year_end_full[i]
    }
  }

  # Construct the date string for parsing (assuming day 01 for simplicity)
  date_to_parse <- paste0("01 ", month_abbr, " ", final_year)

  # Parse with lubridate's dmy function
  lubridate::dmy(date_to_parse)
}

#' Calculate CWT standard compliance at different levels
#'
#' Creates aggregated compliance dataframes for a specified CWT standard,
#' optionally filtered by cancer type. Can calculate at national, alliance,
#' or trust level.
#'
#' @param data Dataframe containing CWT data with columns: STANDARD, CANCER.TYPE,
#'   PERIOD, ORG.CODE, WITHIN.STANDARD, TOTAL.TREATED, and optionally
#'   STAGE.ROUTE and TREATMENT.MODALITY.
#' @param standard_name Character. The CWT standard to filter by, e.g.,
#'   "FDS", "31D", or "62D".
#' @param cancer_type Character vector or NULL. Cancer type(s) to filter by.
#'   If NULL, includes all cancer types.
#' @param level Character. The aggregation level: "national" (all trusts),
#'   "alliance" (alliance trusts only), or "trust" (by individual trust).
#' @return A dataframe with aggregated compliance data. For national and alliance
#'   levels, includes PERIOD, WITHIN.STANDARD, TOTAL.TREATED, and
#'   Patients_Above_Target. For trust level, includes ORG.CODE as well.
#' @examples
#' standard_compliance_calc(cwt_data, "FDS", "Lung", "national")
#' standard_compliance_calc(cwt_data, "62D", NULL, "trust")
#' @export
standard_compliance_calc <- function(data, standard_name, cancer_type = NULL, level= c("national", "alliance", "trust")) {

  level <- match.arg(level)

  data <- data |>
    filter (
      STANDARD == standard_name
    )

  if(!is.null(cancer_type)) {
    data <- data |>
      filter(CANCER.TYPE %in% cancer_type)
  }

  if (standard_name == "FDS") {
    data <- data |>
      filter(
        STAGE.ROUTE == "ALL ROUTES"
      )
  } else if (standard_name == "31D") {
    data <- data |>
      filter (
        STAGE.ROUTE == "ALL STAGES",
        TREATMENT.MODALITY == "ALL MODALITIES"
      )
  } else if (standard_name == "62D") {
    data <- data |>
      filter(
        STAGE.ROUTE == "ALL ROUTES",
        TREATMENT.MODALITY == "ALL MODALITIES"
      )
  }
  
  if (level == "national") {
    
    #Filter to latest monthly data
    latest_period <- max(data$PERIOD, na.rm = TRUE)
    data <- data |> filter(PERIOD == latest_period)
    
    data <- data |>
      group_by(PERIOD) |>
      summarise(
        WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
        TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
        .groups = "drop"
      )
    
    data <- data |>
      mutate(
        Patients_Above_Target 
        = WITHIN.STANDARD - get_cwt_targets_as_patients(standard_name, TOTAL.TREATED)
      )
    
  } else if (level == "alliance") {

    data <- data |> filter(ORG.CODE %in% get_alliance_config("trust_codes"))

    #Filter to latest monthly data
    latest_period <- max(data$PERIOD, na.rm = TRUE)
    data <- data |> filter(PERIOD == latest_period)
    
    data <- data |>
      group_by(PERIOD) |>
      summarise(
        WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
        TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
        .groups = "drop"
      )
    
    data <- data |>
      mutate(
        Patients_Above_Target 
        = WITHIN.STANDARD - get_cwt_targets_as_patients(standard_name, TOTAL.TREATED)
      )
    
  } else if (level == "trust") {
    
    data <- data |>
      group_by(PERIOD, ORG.CODE) |>
      summarise(
        WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
        TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

#' Calculate compliance percentage and add ranking
#'
#' Calculates the compliance percentage from WITHIN.STANDARD and TOTAL.TREATED
#' columns, then ranks organisations within each period by compliance (highest = rank 1).
#'
#' @param data Dataframe containing WITHIN.STANDARD and TOTAL.TREATED columns,
#'   and optionally a PERIOD column for grouping.
#' @return The input dataframe with two additional columns:
#'   \describe{
#'     \item{Compliance_\%}{Compliance percentage (0-100), or NA if TOTAL.TREATED is 0}
#'     \item{RANK}{Dense rank within each period, with rank 1 being highest compliance}
#'   }
#' @examples
#' compliance_df <- add_compliance_rank(cwt_data)
#' @export
add_compliance_rank <- function(data) {
  data |>
    mutate(`Compliance_%` = if_else(
      TOTAL.TREATED > 0,
      (WITHIN.STANDARD/TOTAL.TREATED)*100,
      NA_real_
    )
    ) |>
    group_by(PERIOD) |>
    mutate(
      RANK = (dense_rank(desc(`Compliance_%`))) 
    ) |>
    ungroup()
}


#' Calculate rank change from previous period
#'
#' Calculates the change in ranking from the previous period for each organisation,
#' and returns only the latest period's data with rank movement indicators.
#'
#' @param data Dataframe containing ORG.CODE, PERIOD, and RANK columns.
#'   Should be output from \code{\link{add_compliance_rank}}.
#' @return A dataframe filtered to the latest period, containing:
#'   \describe{
#'     \item{PERIOD}{The reporting period}
#'     \item{ORG.CODE}{Organisation code}
#'     \item{WITHIN.STANDARD}{Patients within standard}
#'     \item{TOTAL.TREATED}{Total patients treated}
#'     \item{Compliance_\%}{Compliance percentage}
#'     \item{RANK}{Current period rank}
#'     \item{PREV_RANK}{Previous period rank}
#'     \item{RANK_DIFF}{Difference (positive = improved)}
#'     \item{RANK_ARROW}{Display string with arrow, e.g. "↑ 2", "↓ 1", "→ 0"}
#'   }
#' @examples
#' ranked_data <- add_compliance_rank(cwt_data)
#' rank_changes <- add_rank_change(ranked_data)
#' @export
add_rank_change <- function(data) {
  data_ranked <- data |>
    arrange(ORG.CODE, PERIOD) |>
    group_by(ORG.CODE) |>
    mutate (
      PREV_RANK = lag(RANK),
      RANK_DIFF = PREV_RANK - RANK,
      RANK_ARROW = case_when(
        is.na(PREV_RANK) ~ "",
        RANK_DIFF > 0    ~ paste0("↑ ", RANK_DIFF),
        RANK_DIFF < 0    ~ paste0("↓ ", abs(RANK_DIFF)),
        TRUE             ~ "→ 0"
      )
    ) |>
    ungroup()
  
  latest_period <- max(data_ranked$PERIOD, na.rm = TRUE)
  
  data_ranked |>
    filter(PERIOD == latest_period) |>
    select(
      PERIOD,
      ORG.CODE,
      WITHIN.STANDARD,
      TOTAL.TREATED,
      `Compliance_%`,
      RANK,
      PREV_RANK,
      RANK_DIFF,
      RANK_ARROW
    ) |>
    
    arrange(RANK)
}

#' Create tumour-specific benchmark table
#'
#' Creates a benchmark table for a specific tumour type and CWT standard,
#' showing compliance, patients above target, rank, and rank change for
#' alliance trusts.
#'
#' @param data Dataframe containing CWT data with standard columns.
#' @param standard_name Character. The CWT standard, e.g., "FDS",
#'   "31D", or "62D".
#' @param cancer_type Character. The cancer type to filter by.
#' @param level Character. The aggregation level passed to
#'   \code{\link{standard_compliance_calc}}.
#' @return A dataframe containing alliance trusts with columns:
#'   \describe{
#'     \item{ORG.CODE}{Organisation code}
#'     \item{ORG.NAME}{Organisation name}
#'     \item{Latest FDS Compliance (\%)}{Compliance percentage for latest period}
#'     \item{Difference from the target (no. patients)}{Number of patients above/below target}
#'     \item{National FDS Ranking}{Current rank}
#'     \item{Ranking difference from previous month}{Rank change with arrow indicator}
#'   }
#'   Also carries a `national_provider_count` attribute (total distinct
#'   providers nationally for this standard/cancer type, before filtering to
#'   alliance trusts) — used to annotate the ranking column header downstream.
#' @examples
#' lung_fds_table <- tumour_benchmark_table(cwt_data, "FDS", "Lung", "trust")
#' @export
tumour_benchmark_table <- function(data, standard_name, cancer_type, level, config = NULL) {

  #Filter to specific standard, tumour to calculate compliance, rank, rank_difference
  standard_df <- standard_compliance_calc(data, standard_name, cancer_type, level)

  standard_df <- add_compliance_rank(standard_df)

  standard_rank_df <- add_rank_change(standard_df)


  standard_rank_df <- standard_rank_df |>
    mutate(
      Patients_Above_Target
      = WITHIN.STANDARD - get_cwt_targets_as_patients(standard_name, TOTAL.TREATED)
    )

  all_rank_df <- standard_rank_df |>
    select(
      ORG.CODE,
      PERIOD,
      `Compliance_%`,
      Patients_Above_Target,
      RANK,
      RANK_ARROW
    ) |>

    pivot_wider(
      names_from = PERIOD,
      values_from = c(`Compliance_%`,Patients_Above_Target, RANK, RANK_ARROW)
    )


  sort_col <- names(all_rank_df)[ncol(all_rank_df) - 3]

  all_rank_df <- all_rank_df |>
    arrange(desc(.data[[sort_col]]))

  # One row per distinct provider nationally (any period in the window) —
  # gives the "of X" context for the ranking column headers downstream.
  national_provider_count <- nrow(all_rank_df)

  alliance_trust_codes <- get_alliance_config("trust_codes")
  org_lookup <- get_alliance_config("org_lookup")

  #Filter to alliance trusts
  wm_rank_df <- all_rank_df |>
    filter(ORG.CODE %in% alliance_trust_codes) |>
    left_join(org_lookup |> select(ORG.CODE, ORG.NAME), by = "ORG.CODE") |>
    relocate(ORG.NAME, .after = ORG.CODE)

  #Rename columns
  nc <- ncol(wm_rank_df)
  names(wm_rank_df)[nc - 3] <- "Latest FDS Compliance (%)"
  names(wm_rank_df)[nc - 2] <- "Difference from the target (no. patients)"
  names(wm_rank_df)[nc - 1] <- "National FDS Ranking"
  names(wm_rank_df)[nc] <- "Ranking difference from previous month"

  # Apply provider group ordering if config is provided
  if (!is.null(config)) {
    wm_rank_df <- apply_provider_group_ordering(
      wm_rank_df, config, code_col = "ORG.CODE",
      sort_col = "Latest FDS Compliance (%)", sort_desc = TRUE
    )
  }

  attr(wm_rank_df, "national_provider_count") <- national_provider_count

  return(wm_rank_df)

}

#' Add national and alliance average rows to trust table
#'
#' Prepends rows for national and alliance averages to a trust-level
#' benchmark table, creating a complete comparison view.
#'
#' @param trust_df Dataframe containing trust-level data from
#'   \code{\link{tumour_benchmark_table}}.
#' @param national_df Dataframe containing national-level aggregated data
#'   with `Compliance_%` and `Patients_Above_Target` columns.
#' @param alliance_df Dataframe containing alliance-level aggregated data
#'   with `Compliance_%` and `Patients_Above_Target` columns.
#' @param label_national Character. Label for the national average row.
#'   Default: "National Average".
#' @param label_alliance Character. Label for the alliance average row.
#'   Default: "Alliance Average".
#' @return A dataframe with national and alliance rows prepended to the
#'   trust data. The ORG.CODE column is empty for average rows.
#' @examples
#' combined_table <- add_national_alliance_rows(
#'   trust_df = trust_benchmark,
#'   national_df = national_compliance,
#'   alliance_df = alliance_compliance
#' )
#' @export
add_national_alliance_rows <- function(trust_df,
                                       national_df,
                                       alliance_df,
                                       label_national = "National Average",
                                       label_alliance = "Alliance Average") {
  
  # extract values
  national_comp     <- national_df$`Compliance_%`[1]
  national_patients <- national_df$`Patients_Above_Target`[1]
  
  alliance_comp     <- alliance_df$`Compliance_%`[1]
  alliance_patients <- alliance_df$`Patients_Above_Target`[1]
  
  # enforce character type for benchmark + diff cols
  trust_df$`National FDS Ranking` <- as.character(trust_df$`National FDS Ranking`)
  trust_df$`Ranking difference from previous month` <- as.character(trust_df$`Ranking difference from previous month`)
  
  # build extra rows
  extra_rows <- data.frame(
    ORG.CODE                     = c("", ""),
    ORG.NAME                     = c(label_national, label_alliance),
    `Latest FDS Compliance (%)`  = c(national_comp, alliance_comp),
    `Difference from the target (no. patients)`  = c(national_patients, alliance_patients),
    `National FDS Ranking`       = "",
    `Ranking difference from previous month` = "",
    check.names = FALSE
  )
  
  # combine and return
  combined <- bind_rows(extra_rows, trust_df)
  return(combined)
}

#' Calculate FDS compliance by pathway end reason
#'
#' Calculates compliance broken down by pathway end reason (diagnosis of cancer
#' vs ruling out of cancer), showing how each trust performs for each outcome type.
#'
#' @param data Dataframe containing detailed FDS data with columns:
#'   Cancer.Report.Category, Month, Provider.Code.FDS, Pathway.End.Reason,
#'   within_standard, NoPatients.
#' @param cancer_type Character vector or NULL. Cancer type(s) to filter by.
#'   If NULL, includes all cancer types.
#' @return A wide-format dataframe with one row per provider, containing:
#'   \describe{
#'     \item{Provider}{Organisation name}
#'     \item{Diagnosis - Compliance_\%}{Compliance for cancer diagnosis pathway}
#'     \item{Diagnosis - Patients above target}{Patients above/below target for diagnosis}
#'     \item{Ruling out - Compliance_\%}{Compliance for ruling out cancer pathway}
#'     \item{Ruling out - Patients above target}{Patients above/below target for ruling out}
#'   }
#' @examples
#' lung_fds_reasons <- calc_fds_end_reason_compliance(fds_data, "Suspected lung cancer")
#' @export
calc_fds_end_reason_compliance <- function(data, cancer_type = NULL, config = NULL) {
  
  if(!is.null(cancer_type)) {
    data <- data |>
      filter(Cancer.Report.Category %in% cancer_type)
  }
    
  data <- data |>
    group_by(Month, Provider.Code.FDS,Pathway.End.Reason) |>
    summarise(
      within_standard = sum(within_standard, na.rm = TRUE),
      NoPatients = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    ) |>
    
    mutate(`Compliance_%` = if_else(
      NoPatients > 0,
      (within_standard/NoPatients)*100,
      NA_real_
    )
    ) |>
    
    mutate(
      Patients_Above_Target
      = within_standard - get_cwt_targets_as_patients("28-day FDS", NoPatients)
    )

  org_lookup <- get_alliance_config("org_lookup")

  # Add Trust names to the data frame
  data <- data |>
    left_join(org_lookup, by = c("Provider.Code.FDS" = "ORG.CODE")) |>
    relocate(ORG.NAME, .after = Provider.Code.FDS)
  
  # Arranging columns by pathway end reason
  fds_pathway_table_df <- data |>
    filter(Pathway.End.Reason %in% c("Diagnosis of cancer", "Ruling out of cancer"))
    
  # Build the four columns explicitly
  fds_pathway_table_df <- fds_pathway_table_df |>
    group_by(ORG.NAME) |>
    summarise(
      `Diagnosis – Compliance_%` =
        first(`Compliance_%`[Pathway.End.Reason == "Diagnosis of cancer"]),
      `Diagnosis – Patients above target` =
        first(Patients_Above_Target[Pathway.End.Reason == "Diagnosis of cancer"]),
      `Ruling out – Compliance_%` =
        first(`Compliance_%`[Pathway.End.Reason == "Ruling out of cancer"]),
      `Ruling out – Patients above target` =
        first(Patients_Above_Target[Pathway.End.Reason == "Ruling out of cancer"]),
      .groups = "drop"
    ) |>
    
    rename(Provider = ORG.NAME) |>
    
    select(
      Provider,
      `Diagnosis – Compliance_%`,
      `Diagnosis – Patients above target`,
      `Ruling out – Compliance_%`,
      `Ruling out – Patients above target`
    )

  # Apply provider group ordering if config is provided
  if (!is.null(config)) {
    fds_pathway_table_df <- apply_provider_group_ordering(
      fds_pathway_table_df, config, code_col = "Provider",
      sort_col = "Diagnosis – Compliance_%", sort_desc = TRUE
    )
  } else {
    fds_pathway_table_df <- fds_pathway_table_df |>
      arrange(desc(`Diagnosis – Compliance_%`))
  }

  fds_pathway_table_df
}

#' Convert date to UK fiscal quarter string
#'
#' Converts a date to a fiscal quarter string in the format "Q1 24/25".
#' The UK fiscal year runs from April to March, so April 2024 is Q1 24/25.
#'
#' @param date Date object or vector of dates to convert.
#' @param start_month Integer. The month that starts the fiscal year.
#'   Default: 4 (April for UK fiscal year).
#' @return Character string in format "Q[1-4] YY/YY", e.g., "Q1 24/25".
#' @examples
#' date_to_fiscal_q(as.Date("2024-04-15"))
#' # Returns "Q1 24/25"
#' date_to_fiscal_q(as.Date("2024-01-15"))
#' # Returns "Q4 23/24"
#' @export
date_to_fiscal_q <- function(date, start_month = 4) {
  fy_start <- ifelse(month(date) >= start_month, year(date), year(date) - 1)
  fy_end   <- fy_start + 1
  fy_quarter <- ((month(date) - start_month) %% 12) %/% 3 + 1
  
  fy_start_str <- substr(fy_start, 3, 4)
  fy_end_str   <- substr(fy_end, 3, 4)
  
  paste0("Q", fy_quarter, " ", fy_start_str, "/", fy_end_str)
}

#' Describe the months of a fiscal quarter up to a given date
#'
#' Labels how much of a fiscal quarter a date reaches, from the quarter's first
#' month to the date's month. Used where a quarter may be only partly populated.
#'
#' @param date Date object (single value).
#' @param start_month Integer. The month that starts the fiscal year.
#'   Default: 4 (April for UK fiscal year).
#' @return Character string, e.g., "Apr" or "Apr-May".
#' @examples
#' fiscal_q_months_to_date(as.Date("2024-05-15"))
#' # Returns "Apr-May"
#' fiscal_q_months_to_date(as.Date("2024-04-15"))
#' # Returns "Apr"
#' @export
fiscal_q_months_to_date <- function(date, start_month = 4) {
  months_into_q <- (month(date) - start_month) %% 3
  q_start <- floor_date(date, "month") %m-% months(months_into_q)

  if (months_into_q == 0) {
    return(format(date, "%b"))
  }
  paste0(format(q_start, "%b"), "-", format(date, "%b"))
}

#####################################
# Staging data group split handling Functions
#####################################

#' Split dataframe by cancer group
#'
#' Splits a staging dataframe into a named list of dataframes,
#' one for each unique cancer group. Useful for iterating over
#' cancer groups in RMarkdown documents.
#'
#' @param df Dataframe containing a `Cancer group` column.
#' @return A named list of dataframes, where names are the unique
#'   cancer group values and each element contains rows for that group.
#' @examples
#' cancer_groups <- split_cancer_group(staging_df)
#' for (group_name in names(cancer_groups)) {
#'   print(group_name)
#'   print(nrow(cancer_groups[[group_name]]))
#' }
#' @export
split_cancer_group <- function(df) {
  group_df <- df %>%
    group_by(`Cancer group`) %>%
    group_split()

  group_names <- df %>%
    distinct(`Cancer group`) %>%
    arrange(`Cancer group`) %>%
    pull(`Cancer group`)

  names(group_df) <- group_names
  
  return(group_df)
}

#' Clean LWBC monthly dataframe
#'
#' Applies standard cleaning transformations to LWBC (Living With and Beyond
#' Cancer) monthly dataframes. This removes the YEAR column, filters out rows
#' with missing MONTH or Cancer Alliance values, replaces NA counts with 0,
#' converts MONTH to Date format, and renames MONTH to Date.
#'
#' @param df A dataframe from LWBC data (HNA or PCSP files).
#' @return A cleaned dataframe with Date column instead of MONTH/YEAR.
#' @examples
#' lwbc_hna_status_df <- clean_lwbc_monthly_df(lwbc_hna_status_df)
#' @export
clean_lwbc_monthly_df <- function(df) {
  df %>%
    select(-YEAR) %>%
    filter(!is.na(MONTH), !is.na(`Cancer Alliance`)) %>%
    mutate(
      `Total Count` = if_else(is.na(`Total Count`), 0, `Total Count`),
      MONTH = as_date(MONTH)
    ) %>%
    rename(Date = MONTH)
}



#' Calculate appropriate upper limit for axis
#'
#' Calculates an appropriate upper limit for a chart axis, based on the magnitude
#' of the maximum value.
#'
#' @param the_max The maximum value in the Y-axis.
#' @return An integer to serve as an appropriate upper axis limit.
#' @examples
#' ylim(0, <- calc_axis_limit(max(my_df$datacol))
#' @export
calc_axis_limit <- function(the_max) {
  case_when(
    the_max < 100 ~ ceiling(the_max / 10) * 10,
    the_max < 1000 ~ ceiling(the_max / 50) * 50,
    TRUE ~ ceiling(the_max / 100) * 100
  )
}

#' Scale the LWBC summary time series' secondary (EOTS) axis
#'
#' The LWBC summary time series charts plot two percentages (% HNAs accepted,
#' % PCSPs completed) on the primary axis and a count (EOTSs completed) on a
#' secondary axis. The count is drawn divided by `scale_factor`, so its peak
#' sits level with the % HNA peak, and the secondary axis multiplies back.
#'
#' A group with no HNAs accepted (all zero), or no rows at all, would make the
#' factor x/0 or 0/0, and ggplot2 then fails to build the secondary axis
#' ("need at least two non-NA values to interpolate"). Those cases fall back
#' to a 0-100% reference for the primary axis, and an unscaled secondary axis
#' when there are no EOTSs either.
#'
#' The primary axis limit covers both percentages, not just % PCSPs — a
#' % HNA (and so the EOTS peak) above the % PCSP peak would otherwise fall
#' outside the limits and be silently dropped from the chart.
#'
#' @param plot_data LWBC plot data for one alliance, with columns
#'   `% HNA's Accepted`, `% PCSP's Completed` and `No. EOTS's Completed`.
#' @return List with `scale_factor` (positive, finite) and `y_max` (the
#'   primary axis upper limit; positive, finite).
lwbc_dual_axis_scale <- function(plot_data) {
  finite_max <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) 0 else max(x)
  }

  hna_max <- finite_max(plot_data$`% HNA's Accepted`)
  pcsp_max <- finite_max(plot_data$`% PCSP's Completed`)
  eots_max <- finite_max(plot_data$`No. EOTS's Completed`)

  pct_max <- max(hna_max, pcsp_max)
  pct_ref <- if (pct_max > 0) pct_max else 100
  anchor <- if (hna_max > 0) hna_max else pct_ref

  list(
    scale_factor = if (eots_max > 0) eots_max / anchor else 1,
    y_max = pct_ref * 1.1
  )
}
