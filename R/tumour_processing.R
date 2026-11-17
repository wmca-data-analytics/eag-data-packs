# Tumour Data Processing Functions
# =================================
# Modular functions for processing tumour-specific data.
# These functions are used by read_tumour_data() to create the standardised
# output structure for each tumour type.

# Source validation utilities
source("R/validation_utils.R")

# =============================================================================
# PROVIDER GROUP ORDERING UTILITIES
# =============================================================================

#' Reorder dataframe rows by provider group then sort column
#'
#' When a tumour config defines cwt_provider_ordering, this function reorders
#' the dataframe so that trusts in Group 1 appear first, then Group 2, etc.,
#' with trusts not in any group sorted last. Within each group, rows are sorted
#' by the specified sort column.
#'
#' @param df Dataframe to reorder
#' @param config Tumour configuration (used to read cwt_provider_ordering)
#' @param code_col Column containing the trust code or display name.
#'   If "ORG.CODE", the value is used directly; otherwise the value is
#'   resolved to a trust code via org_lookup.
#' @param sort_col Column to sort within each group (NULL = keep existing order)
#' @param sort_desc Whether to sort descending within groups (default TRUE)
#' @return The reordered dataframe
apply_provider_group_ordering <- function(df, config, code_col = "ORG.CODE",
                                          sort_col = NULL, sort_desc = TRUE) {
  groups <- config$cwt_provider_ordering
  if (is.null(groups)) {
    return(df)
  }

  # Extract trust code from the specified column
  if (code_col == "ORG.CODE") {
    trust_codes <- df[[code_col]]
  } else {
    org_lookup <- get_alliance_config("org_lookup")
    name_to_code <- setNames(org_lookup$ORG.CODE, org_lookup$ORG.NAME)
    trust_codes <- name_to_code[as.character(df[[code_col]])]
  }

  # Assign group number: trusts in defined groups get their group index,
  # trusts not in any group get length(groups) + 1
  default_group <- length(groups) + 1
  group_num <- rep(default_group, nrow(df))
  for (i in seq_along(groups)) {
    group_num[trust_codes %in% groups[[i]]$codes] <- i
  }

  df$.provider_group <- group_num

  # Callers pass a sort_col by name (e.g. create_standards_table() sorts on
  # "Latest 62-Day Compliance (%)"), but that column only exists if the tumour
  # had data for that standard. For an FDS-only tumour it was never joined, and
  # df[[sort_col]] would return NULL, making order() fail on argument length 0.
  #
  # Degrade to group-order-only sorting rather than erroring: the grouping is
  # the part callers actually depend on for correctness, and within-group order
  # is presentational. Silent because a missing standard is an expected
  # condition, already warned about upstream by validate_df_not_empty_warn().
  if (!is.null(sort_col) && !sort_col %in% names(df)) {
    sort_col <- NULL
  }

  if (!is.null(sort_col)) {
    if (sort_desc) {
      df <- df[order(df$.provider_group, -df[[sort_col]]), ]
    } else {
      df <- df[order(df$.provider_group, df[[sort_col]]), ]
    }
  } else {
    df <- df[order(df$.provider_group), ]
  }

  df$.provider_group <- NULL
  df
}


#' Set ORG.NAME (or similar) factor levels according to provider group ordering
#'
#' Determines the desired level order by group number, then sort_col within
#' group (using the latest period's values if the dataframe has multiple
#' periods), and sets the specified column as a factor with those levels.
#' This ensures downstream ggplot2 facets and x-axis scales follow the
#' group ordering automatically.
#'
#' @param df Dataframe containing the column to factorise
#' @param config Tumour configuration (used to read cwt_provider_ordering)
#' @param code_col Column to set as a factor (default "ORG.NAME")
#' @param sort_col Column to sort within each group (NULL = keep existing order)
#' @param sort_desc Whether to sort descending within groups (default TRUE)
#' @param period_col Column containing the time period (used to find the latest
#'   period for determining sort order). Default "PERIOD".
#' @return The dataframe with code_col converted to a factor with group-ordered levels
set_provider_factor_levels <- function(df, config, code_col = "ORG.NAME",
                                       sort_col = NULL, sort_desc = TRUE,
                                       period_col = "PERIOD") {
  groups <- config$cwt_provider_ordering
  if (is.null(groups)) {
    return(df)
  }

  # Work out level order from the latest period's data (used for sort ranking)
  if (period_col %in% names(df)) {
    latest_df <- df[df[[period_col]] == max(df[[period_col]]), ]
  } else {
    latest_df <- df
  }

  # Get unique values of code_col in the latest period for sort order
  latest_unique <- latest_df[!duplicated(latest_df[[code_col]]), ]

  # Apply group ordering to determine the level order
  latest_ordered <- apply_provider_group_ordering(
    latest_unique, config,
    code_col = code_col,
    sort_col = sort_col,
    sort_desc = sort_desc
  )

  level_order <- latest_ordered[[code_col]]

  # Include any trusts that appear in earlier periods but not the latest,
  # appended at the end (so they don't become NA and cause ptd_spc errors)
  all_values <- unique(df[[code_col]])
  missing_values <- setdiff(all_values, level_order)
  level_order <- c(level_order, missing_values)

  # Set factor levels on the full dataframe
  df[[code_col]] <- factor(df[[code_col]], levels = level_order)

  df
}


# =============================================================================
# PROVIDER GROUP BOUNDARY UTILITIES
# =============================================================================

#' Get row indices where provider groups change (for table hline separators)
#'
#' Given a dataframe already sorted by provider group order, this function
#' returns the row indices of the last row in each group before a boundary.
#' These indices can be passed to flextable::hline() to draw horizontal rules
#' between provider groups.
#'
#' @param df Dataframe with rows already sorted by provider group ordering
#' @param config Tumour configuration (used to read cwt_provider_ordering)
#' @param code_col Column containing the trust code or display name.
#'   If "ORG.CODE", the value is used directly; otherwise the value is
#'   resolved to a trust code via org_lookup.
#' @param offset Number of non-provider rows prepended to the table (e.g. 2 for
#'   benchmark tables with National/Alliance rows). These rows are excluded from
#'   boundary computation, and the offset is added back to the returned indices
#'   so they refer to the correct flextable body row. Default 0.
#' @return Integer vector of row indices where one group ends and the next begins.
#'   Returns integer(0) if cwt_provider_ordering is NULL or only one group exists.
get_provider_group_boundaries <- function(df, config, code_col = "ORG.NAME",
                                          offset = 0) {
  groups <- config$cwt_provider_ordering
  if (is.null(groups)) {
    return(integer(0))
  }

  # Skip non-provider header rows (e.g. National/Alliance) before computing
  if (offset > 0) {
    if (nrow(df) <= offset) {
      return(integer(0))
    }
    df <- df[-(seq_len(offset)), , drop = FALSE]
  }

  # Extract trust code from the specified column
  if (code_col == "ORG.CODE") {
    trust_codes <- df[[code_col]]
  } else {
    org_lookup <- get_alliance_config("org_lookup")
    name_to_code <- setNames(org_lookup$ORG.CODE, org_lookup$ORG.NAME)
    trust_codes <- name_to_code[as.character(df[[code_col]])]
  }

  # Assign group number to each row
  default_group <- length(groups) + 1
  group_num <- rep(default_group, nrow(df))
  for (i in seq_along(groups)) {
    group_num[trust_codes %in% groups[[i]]$codes] <- i
  }

  # Find indices where the group number changes
  boundaries <- which(diff(group_num) != 0)

  if (length(boundaries) == 0) {
    return(integer(0))
  }

  # Add offset back so indices refer to the correct flextable body row
  as.integer(boundaries + offset)
}


# =============================================================================
# PROVIDER GROUP LABEL UTILITIES
# =============================================================================

#' Prepend provider group names to ORG.NAME values for chart labels
#'
#' When a tumour config defines cwt_provider_ordering with named groups,
#' this function prepends the group name in square brackets to each trust's
#' ORG.NAME. Trusts not in any named group are left unprefixed.
#' If ORG.NAME is a factor, level labels are updated to match (preserving order).
#'
#' @param df Dataframe containing an ORG.NAME column
#' @param config Tumour configuration (used to read cwt_provider_ordering)
#' @return The dataframe with prefixed ORG.NAME values
add_provider_group_labels <- function(df, config) {
  groups <- config$cwt_provider_ordering
  if (is.null(groups)) {
    return(df)
  }

  # Check that at least one group has a name field
  has_names <- any(vapply(groups, function(g) !is.null(g$name), logical(1)))
  if (!has_names) {
    return(df)
  }

  # Build a lookup: trust code -> group name
  code_to_group <- character(0)
  for (g in groups) {
    if (!is.null(g$name)) {
      for (code in g$codes) {
        code_to_group[code] <- g$name
      }
    }
  }

  # Build reverse lookup: display name -> trust code (from org_lookup)
  org_lookup <- get_alliance_config("org_lookup")
  name_to_code <- setNames(org_lookup$ORG.CODE, org_lookup$ORG.NAME)

  # Helper to prepend group name to a single ORG.NAME value
  prepend_group <- function(org_name) {
    trust_code <- name_to_code[as.character(org_name)]
    group_name <- code_to_group[trust_code]
    if (!is.na(group_name)) {
      paste0("[", group_name, "] ", org_name)
    } else {
      as.character(org_name)
    }
  }

  if (is.factor(df$ORG.NAME)) {
    # Update factor levels (preserving order) and remap values
    old_levels <- levels(df$ORG.NAME)
    new_levels <- vapply(old_levels, prepend_group, character(1), USE.NAMES = FALSE)
    levels(df$ORG.NAME) <- new_levels
  } else {
    df$ORG.NAME <- vapply(df$ORG.NAME, prepend_group, character(1), USE.NAMES = FALSE)
  }

  df
}


#' Apply provider group labels to all dataframes in a plot result list
#'
#' Iterates over all elements of a list (e.g. $compliance, $riro, $riro_bar)
#' and applies add_provider_group_labels() to any that are dataframes containing
#' an ORG.NAME column. Non-dataframe elements (counts, dates, etc.) are unchanged.
#'
#' @param plot_list List of plot dataframes and other elements
#' @param config Tumour configuration
#' @return The list with group labels applied to relevant dataframes
add_group_labels_to_plot_list <- function(plot_list, config) {
  if (is.null(plot_list) || is.null(config$cwt_provider_ordering)) {
    return(plot_list)
  }

  for (name in names(plot_list)) {
    element <- plot_list[[name]]
    if (is.data.frame(element) && "ORG.NAME" %in% names(element)) {
      plot_list[[name]] <- add_provider_group_labels(element, config)
    }
  }

  plot_list
}


# =============================================================================
# FDS PROCESSING
# =============================================================================

#' Process FDS raw data filtering
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration from load_tumour_config.R
#' @return List with raw FDS dataframes
process_fds_raw <- function(global_data, config) {
  tumour_name <- config$name
  fds_detailed_filter <- get_name_in_dataset(config, "cwt_fds_detailed")

  # cwt_fds_detailed null means the detailed extract has no bucket that
  # distinguishes this tumour (see the null / scalar / array convention in
  # config/tumours.yml's header) — read the public provider/commissioner
  # extract instead. detailed/detailed_comm stay NULL in this branch: they
  # have nothing to filter on, so Rule-in/Rule-out and the FDS Pathway End
  # Reason breakdown (which only ever read detailed data) are skipped for
  # this tumour — see has_fds_detailed in the Rmd template and the
  # fds_end_reason gate in process_benchmarking().
  if (is.null(fds_detailed_filter)) {
    fds_public_filter <- get_name_in_dataset(config, "cwt_fds_public")

    provider_df <- global_data$fds_df %>%
      filter(CANCER.TYPE %in% fds_public_filter)
    validate_filter_result(
      provider_df,
      "CANCER.TYPE",
      fds_public_filter,
      global_data$fds_df,
      paste("FDS public provider data for", tumour_name)
    )

    provider_comm_df <- global_data$cwt_fds_comm_df %>%
      filter(`Cancer Type` %in% fds_public_filter)
    validate_filter_result(
      provider_comm_df,
      "Cancer Type",
      fds_public_filter,
      global_data$cwt_fds_comm_df,
      paste("FDS public commissioner data for", tumour_name)
    )

    return(list(
      provider = provider_df,
      provider_comm = provider_comm_df,
      detailed = NULL,
      detailed_comm = NULL
    ))
  }

  # Detailed provider data (always available)
  detailed_df <- global_data$cwt_d_fds_df %>%
    filter(Cancer.Report.Category %in% fds_detailed_filter)
  validate_filter_result(
    detailed_df,
    "Cancer.Report.Category",
    fds_detailed_filter,
    global_data$cwt_d_fds_df,
    paste("FDS detailed provider data for", tumour_name)
  )

  # Detailed commissioner data (always available)
  detailed_comm_df <- global_data$cwt_d_fds_comm_df %>%
    filter(Cancer.Report.Category %in% fds_detailed_filter)
  validate_filter_result(
    detailed_comm_df,
    "Cancer.Report.Category",
    fds_detailed_filter,
    global_data$cwt_d_fds_comm_df,
    paste("FDS detailed commissioner data for", tumour_name)
  )

  # provider is derived from detailed downstream (see
  # process_fds_summary()/process_fds_plot()); provider_comm is derived here
  # since it has no other consumer besides the FDS Summary slide.
  list(
    provider = NULL,
    provider_comm = derive_fds_provider_comm_from_detailed(detailed_comm_df),
    detailed = detailed_df,
    detailed_comm = detailed_comm_df
  )
}


#' Derive FDS provider-level data from detailed data (mirrors public fds_df shape)
#'
#' Aggregates across all Pathway.End.Reason values (not just rule-in), matching
#' what the public FDS standard represented before public CWT data was lost.
#'
#' @param detailed_df Detailed provider FDS dataframe (fds_raw$detailed)
#' @return Dataframe in standardised format (PERIOD, ORG.NAME, ORG.CODE,
#'   WITHIN.STANDARD, TOTAL.TREATED)
derive_fds_provider_from_detailed <- function(detailed_df) {
  detailed_df %>%
    mutate(
      WITHIN.STANDARD = rowSums(across(c(DaysWithin7, Days8To14, Days15To21, Days22To28)), na.rm = TRUE),
      ORG.CODE = Provider.Code.FDS
    ) %>%
    rename(
      TOTAL.TREATED = NoPatients,
      PERIOD = Date
    )
}


#' Derive an FDS commissioner (alliance-wide) summary from detailed data
#'
#' Mirrors the single-row shape produced by read_in_cwt_public_comm.R for
#' cwt_fds_comm_df (Period, Compliance, "Diff from std"), aggregating across
#' all Pathway.End.Reason values and all ICBs for the latest month.
#'
#' @param detailed_comm_df Detailed commissioner FDS dataframe (fds_raw$detailed_comm)
#' @return Single-row dataframe with Period, Compliance, "Diff from std"
derive_fds_provider_comm_from_detailed <- function(detailed_comm_df) {
  detailed_comm_df %>%
    filter(Date == max(Date)) %>%
    summarise(
      Period = format(max(Date), "%B %Y"),
      Within = sum(rowSums(across(c(DaysWithin7, Days8To14, Days15To21, Days22To28)), na.rm = TRUE)),
      Total = sum(NoPatients, na.rm = TRUE)
    ) %>%
    mutate(
      Compliance = calc_cwt_compliance(Within, Total),
      `Diff from std` = Within - get_cwt_targets_as_patients("fds", Total)
    )
}


#' Process FDS summary data
#'
#' @param fds_raw List with raw FDS dataframes from process_fds_raw()
#' @param config Tumour configuration
#' @return List with FDS summary dataframes
process_fds_summary <- function(fds_raw, config) {
  # fds_raw$provider is non-NULL only when cwt_fds_detailed is null (public-
  # sourced — see process_fds_raw()); otherwise derive from detailed.
  if (is.null(fds_raw$provider)) {
    source_df <- derive_fds_provider_from_detailed(fds_raw$detailed)
  } else {
    source_df <- fds_raw$provider
  }

  # Provider summary
  summary_df <- source_df %>%
    group_by(PERIOD) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(PERIOD == max(PERIOD)) %>%
    mutate(
      COMPLIANCE = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
    )

  list(
    provider = summary_df
  )
}


#' Process FDS rule-in data (provider level)
#'
#' @param fds_raw List with raw FDS dataframes
#' @return List with rule-in FDS dataframes and counts, or NULL when this
#'   tumour has no detailed data (cwt_fds_detailed is null — public-sourced)
process_fds_rule_in <- function(fds_raw) {
  if (is.null(fds_raw$detailed)) {
    return(NULL)
  }

  # Rule-in aggregation
  ri_df <- fds_raw$detailed %>%
    filter(Pathway.End.Reason %in% c("Diagnosis of cancer")) %>%
    group_by(Date, Pathway.End.Reason) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    )

  # Rule-in summary (latest date)
  ri_summary_df <- ri_df %>%
    filter(Date == max(Date)) %>%
    mutate(
      COMPLIANCE = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
    )

  # Rule-in by trust (latest date)
  max_date <- max(fds_raw$detailed$Date)
  ri_trust_df <- fds_raw$detailed %>%
    dplyr::select(Date, ORG.NAME, Pathway.End.Reason, DaysWithin7, Days8To14, Days15To21, Days22To28, NoPatients) %>%
    filter(
      Date == max_date,
      Pathway.End.Reason == "Diagnosis of cancer"
    ) %>%
    group_by(Date, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Latest FDS Compliance (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
    )

  # Trusts above/below target
  trusts_above <- ri_trust_df %>%
    filter(`Latest FDS Compliance (%)` >= CWT_STANDARD_FDS_PC)

  trusts_below <- ri_trust_df %>%
    filter(`Latest FDS Compliance (%)` < CWT_STANDARD_FDS_PC)

  list(
    raw = ri_df,
    summary = ri_summary_df,
    trust = ri_trust_df,
    trusts_above = trusts_above,
    trusts_below = trusts_below,
    count_above = nrow(trusts_above),
    count_below = nrow(trusts_below)
  )
}


#' Process FDS rule-in data (commissioner level)
#'
#' @param fds_raw List with raw FDS dataframes
#' @return List with rule-in commissioner FDS dataframes and counts, or NULL
#'   when this tumour has no detailed data (cwt_fds_detailed is null —
#'   public-sourced)
process_fds_rule_in_comm <- function(fds_raw) {
  if (is.null(fds_raw$detailed_comm)) {
    return(NULL)
  }

  # Rule-in aggregation
  ri_comm_df <- fds_raw$detailed_comm %>%
    filter(Pathway.End.Reason %in% c("Diagnosis of cancer")) %>%
    group_by(Date, Pathway.End.Reason) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    )

  # Rule-in summary (latest date)
  ri_summary_comm_df <- ri_comm_df %>%
    filter(Date == max(Date)) %>%
    mutate(
      COMPLIANCE = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
    )

  # Rule-in by trust (commissioner)
  max_date <- max(fds_raw$detailed_comm$Date)
  ri_trust_comm_df <- fds_raw$detailed_comm %>%
    dplyr::select(Date, ORG.NAME, Pathway.End.Reason, DaysWithin7, Days8To14, Days15To21, Days22To28, NoPatients) %>%
    filter(
      Date == max_date,
      Pathway.End.Reason == "Diagnosis of cancer"
    ) %>%
    group_by(Date, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Latest FDS Compliance (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target_Comm = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
    )

  # Trusts above/below target
  trusts_above_comm <- ri_trust_comm_df %>%
    filter(`Latest FDS Compliance (%)` >= CWT_STANDARD_FDS_PC)

  trusts_below_comm <- ri_trust_comm_df %>%
    filter(`Latest FDS Compliance (%)` < CWT_STANDARD_FDS_PC)

  list(
    raw = ri_comm_df,
    summary = ri_summary_comm_df,
    trust = ri_trust_comm_df,
    trusts_above = trusts_above_comm,
    trusts_below = trusts_below_comm,
    count_above = nrow(trusts_above_comm),
    count_below = nrow(trusts_below_comm)
  )
}


#' Process FDS plot data
#'
#' @param fds_raw List with raw FDS dataframes
#' @param config Tumour configuration (for provider group ordering)
#' @return List with FDS plot dataframes and counts
process_fds_plot <- function(fds_raw, config) {
  # fds_raw$provider is non-NULL only when cwt_fds_detailed is null (public-
  # sourced — see process_fds_raw()); otherwise derive from detailed.
  if (is.null(fds_raw$provider)) {
    provider_source_df <- derive_fds_provider_from_detailed(fds_raw$detailed)
  } else {
    provider_source_df <- fds_raw$provider
  }

  # Total treated plot data
  total_treated_df <- provider_source_df %>%
    group_by(PERIOD) %>%
    summarise(TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE), .groups = "drop")

  # Compliance by period and trust
  compliance_df <- provider_source_df %>%
    group_by(PERIOD, ORG.NAME, ORG.CODE) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `COMPLIANCE (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED),
      Label = ifelse(PERIOD == max(PERIOD), ORG.CODE, NA)
    )

  # Trusts above/below target (latest period)
  max_period <- max(compliance_df$PERIOD)
  trusts_above <- compliance_df %>%
    filter(PERIOD == max_period, `COMPLIANCE (%)` >= CWT_STANDARD_FDS_PC)

  trusts_below <- compliance_df %>%
    filter(PERIOD == max_period, `COMPLIANCE (%)` < CWT_STANDARD_FDS_PC)

  # Rule in/out plot data — only when detailed data exists (cwt_fds_detailed
  # non-null); a public-sourced tumour has nothing to build this from.
  if (is.null(fds_raw$detailed)) {
    riro_df <- NULL
    riro_bar_df <- NULL
  } else {
    riro_df <- fds_raw$detailed %>%
      group_by(Date, ORG.NAME, Pathway.End.Reason) %>%
      summarise(
        WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
        TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `COMPLIANCE (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
        Patients_Above_FDS_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("fds", TOTAL.TREATED)
      ) %>%
      filter(Pathway.End.Reason %in% c("Diagnosis of cancer", "Ruling out of cancer"))

    riro_bar_df <- riro_df %>%
      filter(Date == max(Date))

    riro_df <- set_provider_factor_levels(
      riro_df, config,
      code_col = "ORG.NAME",
      sort_col = "COMPLIANCE (%)", sort_desc = TRUE, period_col = "Date"
    )
    riro_bar_df <- set_provider_factor_levels(
      riro_bar_df, config,
      code_col = "ORG.NAME",
      sort_col = "COMPLIANCE (%)", sort_desc = TRUE, period_col = "Date"
    )
  }

  # Apply provider group ordering to factor levels for chart facet/axis ordering
  compliance_df <- set_provider_factor_levels(
    compliance_df, config,
    code_col = "ORG.NAME",
    sort_col = "COMPLIANCE (%)", sort_desc = TRUE
  )

  list(
    total_treated = total_treated_df,
    compliance = compliance_df,
    trusts_above = trusts_above,
    trusts_below = trusts_below,
    count_above = nrow(trusts_above),
    count_below = nrow(trusts_below),
    riro = riro_df,
    riro_bar = riro_bar_df
  )
}


#' Process FDS table data
#'
#' @param fds_plot List with FDS plot dataframes from process_fds_plot()
#' @param config Tumour configuration (for provider group ordering)
#' @return List with FDS table dataframe and latest date
process_fds_table <- function(fds_plot, config) {
  table_df <- fds_plot$compliance %>%
    dplyr::select(PERIOD, ORG.NAME, `COMPLIANCE (%)`, Patients_Above_FDS_Target) %>%
    filter(PERIOD == max(PERIOD)) %>%
    pivot_wider(names_from = PERIOD, values_from = c(`COMPLIANCE (%)`, Patients_Above_FDS_Target))

  latest_date <- max(fds_plot$compliance$PERIOD)

  names(table_df)[ncol(table_df) - 1] <- "Latest FDS Compliance (%)"
  names(table_df)[ncol(table_df)] <- "Patients_Above_FDS_Target_Latest"

  table_df <- apply_provider_group_ordering(
    table_df, config,
    code_col = "ORG.NAME",
    sort_col = "Latest FDS Compliance (%)", sort_desc = TRUE
  )

  list(
    table = table_df,
    latest_date = latest_date
  )
}


#' Process FDS correlation data
#'
#' @param fds_raw List with raw FDS dataframes
#' @param fds_plot List with FDS plot dataframes
#' @return List with FDS correlation dataframes
process_fds_correlation <- function(fds_raw, fds_plot) {
  # Overall FDS correlation
  overall_df <- fds_plot$compliance %>%
    group_by(PERIOD, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`COMPLIANCE FDS (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED)) %>%
    dplyr::select(PERIOD, ORG.NAME, `COMPLIANCE FDS (%)`)

  # Rule-in/rule-out correlation need detailed data (cwt_fds_detailed
  # non-null); a public-sourced tumour has nothing to build these from.
  if (is.null(fds_raw$detailed)) {
    return(list(overall = overall_df, rule_out = NULL, rule_in = NULL))
  }

  # Rule-out correlation
  rule_out_df <- fds_raw$detailed %>%
    filter(Pathway.End.Reason == "Ruling out of cancer") %>%
    group_by(Date, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`COMPLIANCE FDS RO (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED)) %>%
    dplyr::select(Date, ORG.NAME, `COMPLIANCE FDS RO (%)`)

  # Rule-in correlation
  rule_in_df <- fds_raw$detailed %>%
    filter(Pathway.End.Reason == "Diagnosis of cancer") %>%
    group_by(Date, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(c(DaysWithin7, Days8To14, Days15To21, Days22To28), na.rm = TRUE),
      TOTAL.TREATED = sum(NoPatients, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`COMPLIANCE FDS RI (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED)) %>%
    dplyr::select(Date, ORG.NAME, `COMPLIANCE FDS RI (%)`)

  list(
    overall = overall_df,
    rule_out = rule_out_df,
    rule_in = rule_in_df
  )
}


# =============================================================================
# 31-DAY PROCESSING
# =============================================================================

#' Process 31-day raw data filtering
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return List with raw 31-day dataframes
process_31_day_raw <- function(global_data, config) {
  tumour_name <- config$name
  day_31_detailed_filter <- get_name_in_dataset(config, "cwt_31day_detailed")

  # cwt_31day_detailed null means the detailed extract has no bucket that
  # distinguishes this tumour — read the public extract instead (see the
  # null / scalar / array convention in config/tumours.yml's header).
  if (is.null(day_31_detailed_filter)) {
    day_31_public_filter <- get_name_in_dataset(config, "cwt_31day_public")
    public_df <- global_data$day_31_df %>%
      filter(CANCER.TYPE %in% day_31_public_filter)
    validate_filter_result(
      public_df,
      "CANCER.TYPE",
      day_31_public_filter,
      global_data$day_31_df,
      paste("31-day public data for", tumour_name)
    )
    return(list(public = public_df, detailed = NULL))
  }

  detailed_df <- global_data$cwt_d_31_day_df %>%
    filter(CancerReportCategory %in% day_31_detailed_filter)
  detailed_df <- validate_df_not_empty_warn(
    detailed_df,
    paste("31-day detailed data for", tumour_name)
  )

  list(public = NULL, detailed = detailed_df)
}


#' Process 31-day data when derived from detailed (Brain & CNS)
#'
#' @param raw_31 List with raw 31-day dataframes
#' @return Dataframe in standardised format
derive_31_day_from_detailed <- function(raw_31) {
  if (is.null(raw_31$detailed)) {
    return(NULL)
  }

  raw_31$detailed %>%
    group_by(AdmittedCare, TreatmentModality, ORG.NAME, Date) %>%
    mutate(
      WITHIN.STANDARD = DaysWithin31,
      ORG.CODE = ProviderCode
    ) %>%
    rename(
      TOTAL.TREATED = NoTreated,
      PERIOD = Date
    ) %>%
    ungroup() %>%
    select(-Trust_Name)
}


#' Process 31-day plot data
#'
#' @param raw_31 List with raw 31-day dataframes
#' @param config Tumour configuration
#' @return List with 31-day plot data
process_31_day_plot <- function(raw_31, config) {
  # raw_31$public is non-NULL only when cwt_31day_detailed is null (public-
  # sourced — see process_31_day_raw()); otherwise derive from detailed.
  if (is.null(raw_31$public)) {
    source_df <- derive_31_day_from_detailed(raw_31)
  } else {
    source_df <- raw_31$public
  }

  if (is.null(source_df) || nrow(source_df) == 0) {
    return(NULL)
  }

  plot_df <- source_df %>%
    group_by(PERIOD, ORG.NAME, ORG.CODE) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `COMPLIANCE (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_31_Day_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("31 day", TOTAL.TREATED),
      Label = ifelse(PERIOD == max(PERIOD), ORG.CODE, NA)
    )

  # Apply provider group ordering to factor levels for chart facet/axis ordering
  plot_df <- set_provider_factor_levels(
    plot_df, config,
    code_col = "ORG.NAME",
    sort_col = "COMPLIANCE (%)", sort_desc = TRUE
  )

  list(plot = plot_df)
}


#' Process 31-day table data
#'
#' @param plot_31 List with 31-day plot data from process_31_day_plot()
#' @param config Tumour configuration (for provider group ordering)
#' @return List with 31-day table dataframe and latest date
process_31_day_table <- function(plot_31, config) {
  if (is.null(plot_31$plot)) {
    return(list(table = NULL, latest_date = NULL))
  }

  table_df <- plot_31$plot %>%
    dplyr::select(PERIOD, ORG.NAME, `COMPLIANCE (%)`, Patients_Above_31_Day_Target) %>%
    filter(PERIOD == max(PERIOD)) %>%
    pivot_wider(names_from = PERIOD, values_from = c(`COMPLIANCE (%)`, Patients_Above_31_Day_Target))

  latest_date <- max(plot_31$plot$PERIOD)

  names(table_df)[ncol(table_df) - 1] <- "Latest 31-Day Compliance (%)"
  names(table_df)[ncol(table_df)] <- "Patients_Above_31_Day_Target_Latest"

  table_df <- apply_provider_group_ordering(
    table_df, config,
    code_col = "ORG.NAME",
    sort_col = "Latest 31-Day Compliance (%)", sort_desc = TRUE
  )

  list(
    table = table_df,
    latest_date = latest_date
  )
}


# =============================================================================
# 62-DAY PROCESSING
# =============================================================================

#' Process 62-day raw data filtering
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return List with raw 62-day dataframes
process_62_day_raw <- function(global_data, config) {
  tumour_name <- config$name
  day_62_detailed_filter <- get_name_in_dataset(config, "cwt_62day_detailed")

  # cwt_62day_detailed null means the detailed extract has no bucket that
  # distinguishes this tumour — read the public provider/commissioner extract
  # instead (see the null / scalar / array convention in config/tumours.yml's
  # header). cwt_62day_public_comm is the public release's own commissioner-
  # level value — a separate field from cwt_62day_public (the provider-level
  # value), because that's how the public release itself splits them; see
  # config/tumours.yml's header for why that field, specifically, needed to
  # come back after being removed as dead.
  if (is.null(day_62_detailed_filter)) {
    day_62_public_filter <- get_name_in_dataset(config, "cwt_62day_public")
    public_df <- global_data$day_62_df %>%
      filter(CANCER.TYPE %in% day_62_public_filter)
    validate_filter_result(
      public_df,
      "CANCER.TYPE",
      day_62_public_filter,
      global_data$day_62_df,
      paste("62-day public data for", tumour_name)
    )

    day_62_comm_filter <- get_name_in_dataset(config, "cwt_62day_public_comm")
    comm_df <- global_data$cwt_62d_comm_df %>%
      filter(`Cancer Type` %in% day_62_comm_filter)
    validate_filter_result(
      comm_df,
      "Cancer Type",
      day_62_comm_filter,
      global_data$cwt_62d_comm_df,
      paste("62-day public commissioner data for", tumour_name)
    )

    return(list(public = public_df, detailed = NULL, comm = comm_df))
  }

  detailed_df <- global_data$cwt_d_62_day_df %>%
    filter(CancerReportCategory %in% day_62_detailed_filter)
  detailed_df <- validate_df_not_empty_warn(
    detailed_df,
    paste("62-day detailed data for", tumour_name)
  )

  # Derive a commissioner-level (alliance-wide) summary from detailed data,
  # since it has no other consumer besides the 62-Day Summary slide.
  detailed_comm_df <- global_data$cwt_d_62_day_comm_df %>%
    filter(CancerReportCategory %in% day_62_detailed_filter)
  detailed_comm_df <- validate_df_not_empty_warn(
    detailed_comm_df,
    paste("62-day detailed commissioner data for", tumour_name)
  )

  list(
    public = NULL,
    detailed = detailed_df,
    comm = derive_62_day_comm_from_detailed(detailed_comm_df)
  )
}


#' Derive a 62-day commissioner (alliance-wide) summary from detailed data
#'
#' Mirrors the single-row shape produced by read_in_cwt_public_comm.R for
#' cwt_62d_comm_df (Period, Compliance, "Diff from std"), aggregating across
#' all ICBs for the latest month.
#'
#' @param detailed_comm_df Detailed commissioner 62-day dataframe, or NULL when
#'   the tumour has no rows in the detailed extract
#' @return Single-row dataframe with Period, Compliance, "Diff from std", or
#'   NULL if detailed_comm_df is NULL
derive_62_day_comm_from_detailed <- function(detailed_comm_df) {
  # The caller filters with validate_df_not_empty_warn(), which WARNS and
  # returns NULL on an empty result — unlike validate_filter_result(), used on
  # the public paths, which stops. That asymmetry is deliberate: detailed data
  # is optional per tumour, public data is not. It does mean anything consuming
  # a warn-validated dataframe must handle NULL, or dplyr fails with
  # "no applicable method for 'filter' applied to an object of class NULL".
  #
  # Reachable for any tumour with no rows in the 62-day detailed extract, e.g.
  # FDS-only referral routes such as non-specific symptoms: those patients
  # convert to a tumour site before treatment, so no 62-day record is ever
  # filed against the referral route itself.
  #
  # Returning NULL rather than an empty frame matches derive_31_day_from_detailed()
  # and derive_62_day_from_detailed(), and the downstream 62-day processors all
  # test for NULL already.
  if (is.null(detailed_comm_df)) {
    return(NULL)
  }

  detailed_comm_df %>%
    filter(Date == max(Date)) %>%
    summarise(
      Period = format(max(Date), "%B %Y"),
      Within = sum(rowSums(across(c(DaysWithin31, Days32to38, Days39to48, Days49to62)), na.rm = TRUE)),
      Total = sum(NoTreated, na.rm = TRUE)
    ) %>%
    mutate(
      Compliance = calc_cwt_compliance(Within, Total),
      `Diff from std` = Within - get_cwt_targets_as_patients("62 day", Total)
    )
}


#' Process 62-day data when derived from detailed (Brain & CNS)
#'
#' @param raw_62 List with raw 62-day dataframes
#' @return Dataframe in standardised format
derive_62_day_from_detailed <- function(raw_62) {
  if (is.null(raw_62$detailed)) {
    return(NULL)
  }

  raw_62$detailed %>%
    group_by(AdmittedCare, TreatmentModality, ORG.NAME, Date) %>%
    mutate(
      WITHIN.STANDARD = (DaysWithin31 + Days32to38 + Days39to48 + Days49to62),
      ORG.CODE = AccountableProvider
    ) %>%
    rename(
      TOTAL.TREATED = NoTreated,
      PERIOD = Date
    ) %>%
    ungroup() %>%
    select(-Trust_Name)
}


#' Process 62-day summary data
#'
#' @param raw_62 List with raw 62-day dataframes
#' @param config Tumour configuration
#' @return List with 62-day summary dataframe
process_62_day_summary <- function(raw_62, config) {
  # raw_62$public is non-NULL only when cwt_62day_detailed is null (public-
  # sourced — see process_62_day_raw()); otherwise derive from detailed.
  if (is.null(raw_62$public)) {
    source_df <- derive_62_day_from_detailed(raw_62)
  } else {
    source_df <- raw_62$public
  }

  if (is.null(source_df) || nrow(source_df) == 0) {
    return(list(provider = NULL))
  }

  summary_df <- source_df %>%
    group_by(PERIOD) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(PERIOD == max(PERIOD)) %>%
    mutate(
      COMPLIANCE = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_62_Day_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("62 day", TOTAL.TREATED)
    )

  list(provider = summary_df)
}


#' Process 62-day plot data
#'
#' @param raw_62 List with raw 62-day dataframes
#' @param config Tumour configuration
#' @return List with 62-day plot dataframes and counts
process_62_day_plot <- function(raw_62, config) {
  # raw_62$public is non-NULL only when cwt_62day_detailed is null (public-
  # sourced — see process_62_day_raw()); otherwise derive from detailed.
  if (is.null(raw_62$public)) {
    source_df <- derive_62_day_from_detailed(raw_62)
  } else {
    source_df <- raw_62$public
  }

  if (is.null(source_df) || nrow(source_df) == 0) {
    return(NULL)
  }

  # Total treated plot data
  total_treated_df <- source_df %>%
    group_by(PERIOD) %>%
    summarise(TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE), .groups = "drop")

  # Compliance plot data
  compliance_df <- source_df %>%
    group_by(PERIOD, ORG.NAME, ORG.CODE) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `COMPLIANCE (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED),
      Patients_Above_62_Day_Target = WITHIN.STANDARD - get_cwt_targets_as_patients("62 day", TOTAL.TREATED),
      Label = ifelse(PERIOD == max(PERIOD), ORG.CODE, NA)
    )

  # Trusts above/below target
  max_period <- max(compliance_df$PERIOD)
  trusts_above <- compliance_df %>%
    filter(PERIOD == max_period, `COMPLIANCE (%)` >= CWT_STANDARD_62DAY_PC)

  trusts_below <- compliance_df %>%
    filter(PERIOD == max_period, `COMPLIANCE (%)` < CWT_STANDARD_62DAY_PC)

  # Apply provider group ordering to factor levels for chart facet/axis ordering
  compliance_df <- set_provider_factor_levels(
    compliance_df, config,
    code_col = "ORG.NAME",
    sort_col = "COMPLIANCE (%)", sort_desc = TRUE
  )

  list(
    total_treated = total_treated_df,
    compliance = compliance_df,
    trusts_above = trusts_above,
    trusts_below = trusts_below,
    count_above = nrow(trusts_above),
    count_below = nrow(trusts_below)
  )
}


#' Process 62-day table data
#'
#' @param plot_62 List with 62-day plot data from process_62_day_plot()
#' @param config Tumour configuration (for provider group ordering)
#' @return List with 62-day table dataframe and latest date
process_62_day_table <- function(plot_62, config) {
  if (is.null(plot_62) || is.null(plot_62$compliance)) {
    return(list(table = NULL, latest_date = NULL))
  }

  table_df <- plot_62$compliance %>%
    dplyr::select(PERIOD, ORG.NAME, `COMPLIANCE (%)`, Patients_Above_62_Day_Target) %>%
    filter(PERIOD == max(PERIOD)) %>%
    pivot_wider(names_from = PERIOD, values_from = c(`COMPLIANCE (%)`, Patients_Above_62_Day_Target))

  latest_date <- max(plot_62$compliance$PERIOD)

  names(table_df)[ncol(table_df) - 1] <- "Latest 62-Day Compliance (%)"
  names(table_df)[ncol(table_df)] <- "Patients_Above_62_Day_Target_Latest"

  table_df <- apply_provider_group_ordering(
    table_df, config,
    code_col = "ORG.NAME",
    sort_col = "Latest 62-Day Compliance (%)", sort_desc = TRUE
  )

  list(
    table = table_df,
    latest_date = latest_date
  )
}


#' Process 62-day correlation data
#'
#' @param raw_62 List with raw 62-day dataframes
#' @param config Tumour configuration
#' @return 62-day correlation dataframe
process_62_day_correlation <- function(raw_62, config) {
  # raw_62$public is non-NULL only when cwt_62day_detailed is null (public-
  # sourced — see process_62_day_raw()); otherwise derive from detailed.
  if (is.null(raw_62$public)) {
    source_df <- derive_62_day_from_detailed(raw_62)
  } else {
    source_df <- raw_62$public
  }

  if (is.null(source_df) || nrow(source_df) == 0) {
    return(NULL)
  }

  source_df %>%
    group_by(PERIOD, ORG.NAME) %>%
    summarise(
      WITHIN.STANDARD = sum(WITHIN.STANDARD, na.rm = TRUE),
      TOTAL.TREATED = sum(TOTAL.TREATED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`COMPLIANCE 62-day (%)` = calc_cwt_compliance(WITHIN.STANDARD, TOTAL.TREATED)) %>%
    dplyr::select(PERIOD, ORG.NAME, `COMPLIANCE 62-day (%)`)
}


# =============================================================================
# STAGING PROCESSING
# =============================================================================

#' Process staging completeness raw data
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return Staging completeness dataframe or NULL if not available
process_staging_completeness_raw <- function(global_data, config) {
  if (is.null(config$staging_completeness)) {
    return(NULL)
  }

  sc_filter <- get_name_in_dataset(config, "staging_completeness")
  result <- global_data$stage_completeness_df %>%
    filter(CANCER_GROUP_NM %in% sc_filter)

  # Use soft validation - staging completeness is optional
  validate_df_not_empty_warn(
    result,
    paste("staging completeness data for", config$name)
  )
}


#' Process staging completeness plot data
#'
#' @param sc_raw Staging completeness raw dataframe
#' @return List with staging completeness plot data
process_staging_completeness_plot <- function(sc_raw) {
  if (is.null(sc_raw) || nrow(sc_raw) == 0) {
    return(NULL)
  }

  plot_df <- sc_raw %>%
    group_by(DIAGNOSIS_QUARTER, ORG.NAME) %>%
    summarise(
      MDT_STAGE = sum(MDT_STAGE, na.rm = TRUE),
      MDT_DISCUSSED = sum(MDT_DISCUSSED, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Stage Completeness %` = calc_staging_completeness(MDT_STAGE, MDT_DISCUSSED),
      Patients_Above_Target = MDT_STAGE - (STAGING_COMPLETENESS_TARGET * MDT_DISCUSSED)
    )

  # Get previous quarter
  latest_q <- max(plot_df$DIAGNOSIS_QUARTER, na.rm = TRUE)
  prev_q_date <- as.Date(as.yearqtr(latest_q) - 0.25, frac = 0)

  figure_df <- plot_df %>%
    filter(DIAGNOSIS_QUARTER == prev_q_date) %>%
    arrange(desc(`Stage Completeness %`))

  list(
    plot = plot_df,
    figure = figure_df,
    current_quarter = as.yearqtr(prev_q_date)
  )
}


#' Process staging completeness for a split tumour (per-group slides)
#'
#' Iterates the tumour's staging_completeness split values (see the null /
#' scalar / array convention in config/tumours.yml's header) and, for each,
#' filters CANCER_GROUP_NM directly using that raw value and produces a
#' completeness plot. The raw value doubles as the result's key (and the
#' slide-title suffix) — there is no separate display-name mechanism.
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return Named list keyed by the raw split value, or NULL if not split
process_staging_completeness_subtypes <- function(global_data, config) {
  if (!has_split(config, "staging_completeness")) {
    return(NULL)
  }

  result <- list()

  for (group_val in config$staging_completeness) {
    sc_raw <- global_data$stage_completeness_df %>%
      filter(CANCER_GROUP_NM %in% group_val)

    validate_df_not_empty_warn(
      sc_raw,
      paste0("staging completeness data for ", config$name, " (", group_val, ")")
    )

    result[[group_val]] <- process_staging_completeness_plot(sc_raw)
  }

  if (length(result) == 0) NULL else result
}


#' Process staging raw data for demographics
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return Staging dataframe or NULL if not available
process_staging_raw <- function(global_data, config) {
  if (is.null(config$staging_demographics) || has_split(config, "staging_demographics")) {
    return(NULL)
  }

  staging_filter <- get_name_in_dataset(config, "staging_demographics")
  result <- global_data$stage_df %>%
    filter(`Cancer group` %in% staging_filter)
  validate_df_not_empty_warn(
    result,
    paste("staging demographics data for", config$name)
  )
}


#' Process early diagnosis staging data (alliance level)
#'
#' @param stage_raw Staging raw dataframe
#' @return Early diagnosis dataframe
process_early_diagnosis_alliance <- function(stage_raw) {
  if (is.null(stage_raw) || nrow(stage_raw) == 0) {
    return(NULL)
  }

  # dplyr::if_else(), not base ifelse(): base ifelse() on a zero-row input
  # returns logical(0) regardless of the yes/no argument types (it reuses the
  # `test` vector's own type as the answer container) — a real crash once any
  # split can filter down to zero matching rows (e.g. a Geography
  # type/Breakdown/Demographic combination this particular split group has no
  # rows for), since ptd_spc() downstream requires a numeric column.
  # if_else() type-checks against its true/false arguments directly and has
  # no such gap.
  result <- stage_raw %>%
    dplyr::select(`Geography type`, Date, `Cancer group`, Metric, Breakdown, Demographic, `Statistic (12m)`) %>%
    filter(
      `Geography type` == "Cancer alliance",
      Date >= "2024-10-01",
      Metric == "Early stage proportion",
      Breakdown == "Total",
      Demographic == "Total"
    )

  # This standard's filter can legitimately match zero rows for a tumour/
  # split-group whose own data simply doesn't have an alliance-level "Total"
  # cut (e.g. a split value that exists in the raw extract but not at this
  # aggregation) — return NULL, same contract as the empty-input guard above,
  # rather than a 0-row frame the Rmd template's unconditional ptd_spc() call
  # would still error on.
  if (nrow(result) == 0) {
    return(NULL)
  }

  result %>%
    mutate(`Statistic (12m)` = dplyr::if_else(is.na(`Statistic (12m)`), 0, `Statistic (12m)`))
}


#' Process early diagnosis staging data (trust level)
#'
#' @param stage_raw Staging raw dataframe
#' @return Early diagnosis trust dataframe
process_early_diagnosis_trust <- function(stage_raw) {
  if (is.null(stage_raw) || nrow(stage_raw) == 0) {
    return(NULL)
  }

  # See process_early_diagnosis_alliance() for why if_else(), not ifelse(),
  # and why a 0-row post-filter result returns NULL.
  result <- stage_raw %>%
    dplyr::select(`Geography type`, ORG.NAME, Date, `Cancer group`, Metric, Breakdown, Demographic, `Statistic (12m)`) %>%
    filter(
      `Geography type` == "Provider",
      Date >= "2024-10-01",
      Metric == "Early stage proportion",
      Breakdown == "Total",
      Demographic == "Total"
    )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result %>%
    mutate(`Statistic (12m)` = dplyr::if_else(is.na(`Statistic (12m)`), 0, `Statistic (12m)`))
}


#' Process early diagnosis staging data by demographic breakdown
#'
#' @param stage_raw Staging raw dataframe
#' @param breakdown_type Type of breakdown ("Gender", "Age-group", "Ethnicity", "Deprivation", "Route to Diagnosis")
#' @return Early diagnosis demographic dataframe
process_early_diagnosis_demographic <- function(stage_raw, breakdown_type) {
  if (is.null(stage_raw) || nrow(stage_raw) == 0) {
    return(NULL)
  }

  # See process_early_diagnosis_alliance() for why if_else(), not ifelse(),
  # and why a 0-row post-filter result returns NULL.
  result <- stage_raw %>%
    dplyr::select(`Geography type`, ORG.NAME, Date, `Cancer group`, Metric, Breakdown, Demographic, `Statistic (12m)`) %>%
    filter(
      `Geography type` == "Cancer alliance",
      Date >= "2024-10-01",
      Metric == "Early stage proportion",
      Breakdown == breakdown_type
    )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result %>%
    mutate(`Statistic (12m)` = dplyr::if_else(is.na(`Statistic (12m)`), 0, `Statistic (12m)`))
}


#' Process all staging demographics
#'
#' @param stage_raw Staging raw dataframe
#' @param config Tumour configuration
#' @return List with all demographic breakdowns
process_staging_demographics <- function(stage_raw, config) {
  if (is.null(config$staging_demographics) || is.null(stage_raw)) {
    return(NULL)
  }

  list(
    overall = process_early_diagnosis_alliance(stage_raw),
    trust = process_early_diagnosis_trust(stage_raw),
    gender = process_early_diagnosis_demographic(stage_raw, "Gender"),
    age = process_early_diagnosis_demographic(stage_raw, "Age-group"),
    ethnicity = process_early_diagnosis_demographic(stage_raw, "Ethnicity"),
    deprivation = process_early_diagnosis_demographic(stage_raw, "Deprivation"),
    route = process_early_diagnosis_demographic(stage_raw, "Route to Diagnosis")
  )
}


#' Process staging demographics for a split tumour (per-group slides)
#'
#' Returns data organised by demographic type, keyed by the raw
#' staging_demographics split value (see the null / scalar / array convention
#' in config/tumours.yml's header) — the raw value doubles as the display
#' name/slide-title suffix. Allows RMarkdown to iterate:
#' for (grp in names(tumour_data$sub_types$overall)) { ... }
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return List with demographic breakdowns organised by type, keyed by the
#'   raw split value, or NULL if not split
process_staging_with_subtypes <- function(global_data, config) {
  if (!has_split(config, "staging_demographics")) {
    return(NULL)
  }

  # Initialize result structure by demographic type
  result <- list(
    overall = list(),
    trust = list(),
    gender = list(),
    age = list(),
    ethnicity = list(),
    deprivation = list(),
    route = list()
  )

  for (group_val in config$staging_demographics) {
    stage_raw <- global_data$stage_df %>%
      filter(`Cancer group` %in% group_val)

    # Process each demographic and add to result using the raw value as key
    result$overall[[group_val]] <- process_early_diagnosis_alliance(stage_raw)
    result$trust[[group_val]] <- process_early_diagnosis_trust(stage_raw)
    result$gender[[group_val]] <- process_early_diagnosis_demographic(stage_raw, "Gender")
    result$age[[group_val]] <- process_early_diagnosis_demographic(stage_raw, "Age-group")
    result$ethnicity[[group_val]] <- process_early_diagnosis_demographic(stage_raw, "Ethnicity")
    result$deprivation[[group_val]] <- process_early_diagnosis_demographic(stage_raw, "Deprivation")
    result$route[[group_val]] <- process_early_diagnosis_demographic(stage_raw, "Route to Diagnosis")
  }

  result
}


# =============================================================================
# BENCHMARKING PROCESSING
# =============================================================================

#' Process benchmarking tables
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return List with benchmarking tables
process_benchmarking <- function(global_data, config) {
  result <- list()

  # Each standard is gated on two independent conditions:
  #   cwt_public_data_exists_for_benchmarking — does the public workbook hold
  #                                              this tumour?
  #   includes_benchmarking()                 — do we want this standard's
  #                                              slide?
  # See includes_benchmarking() in load_tumour_config.R for why these are
  # separate. A standard that is gated out is left absent from `result`, which
  # the template checks before rendering the corresponding slide.
  has_public_benchmarking <- isTRUE(config$cwt_public_data_exists_for_benchmarking)

  # 28-day FDS benchmarking
  if (has_public_benchmarking && includes_benchmarking(config, "fds")) {
    result$day_28 <- process_benchmark_standard_split(
      global_data$latest_public_2_months_df,
      standard = "FDS",
      cancer_type = get_name_in_dataset(config, "cwt_fds_public"),
      config = config
    )
  }

  # 31-day benchmarking
  if (has_public_benchmarking && includes_benchmarking(config, "31day")) {
    result$day_31 <- process_benchmark_standard_split(
      global_data$latest_public_2_months_df,
      standard = "31D",
      cancer_type = get_name_in_dataset(config, "cwt_31day_public"),
      config = config
    )
  }

  # 62-day benchmarking
  if (has_public_benchmarking && includes_benchmarking(config, "62day")) {
    result$day_62 <- process_benchmark_standard_split(
      global_data$latest_public_2_months_df,
      standard = "62D",
      cancer_type = get_name_in_dataset(config, "cwt_62day_public"),
      config = config
    )
  }

  # FDS end reason — gated on cwt_fds_detailed, not
  # cwt_public_data_exists_for_benchmarking. Unlike the three blocks above —
  # which read the public benchmarking extract (latest_public_2_months_df)
  # and so use public category names — this reads latest_cwt_d_df, which
  # read_in_cwt_extract_provider.R derives from the detailed extract. It
  # therefore filters Cancer.Report.Category and must use the detailed name —
  # a public-sourced tumour (cwt_fds_detailed null) has no detailed category
  # to filter on, so this is omitted for it, same as Rule-in/Rule-out.
  if (!is.null(config$cwt_fds_detailed)) {
    result$fds_end_reason <- calc_fds_end_reason_compliance(
      global_data$latest_cwt_d_df,
      cancer_type = get_name_in_dataset(config, "cwt_fds_detailed"),
      config = config
    )
  }

  result
}


#' Process a benchmark standard, splitting by group when the filter is split
#'
#' Most tumours pass a single cancer_type value straight through to
#' process_benchmark_standard(). A tumour whose cwt_*_public field is split
#' (see the null / scalar / array convention in config/tumours.yml's header —
#' e.g. Urology's 31-day/62-day benchmarking splitting into Urological /
#' Prostate) instead gets ONE process_benchmark_standard() call PER group,
#' returned as a named list keyed by the raw group value — a genuine split
#' into separate ranked tables, not the single combined %in% match a plain
#' multi-value filter would otherwise produce.
#'
#' @param data Benchmark data (latest_public_2_months_df)
#' @param standard Standard type ("FDS", "31D", "62D")
#' @param cancer_type Character vector: one value (no split) or several
#'   (split) raw categories
#' @param config Tumour configuration (for provider group ordering)
#' @return A single benchmark table (process_benchmark_standard()'s result),
#'   or — when split — a named list of such tables keyed by raw group value
process_benchmark_standard_split <- function(data, standard, cancer_type, config) {
  if (length(cancer_type) <= 1) {
    return(process_benchmark_standard(data, standard, cancer_type, config))
  }

  result <- list()
  for (group_val in cancer_type) {
    result[[group_val]] <- process_benchmark_standard(data, standard, group_val, config)
  }
  result
}


#' Process a single benchmark standard (trust, national, alliance)
#'
#' @param data Benchmark data
#' @param standard Standard type
#' @param cancer_type Cancer type filter
#' @param config Tumour configuration (for provider group ordering)
#' @return Combined benchmark table, carrying a `national_provider_count`
#'   attribute (see \code{\link{tumour_benchmark_table}})
process_benchmark_standard <- function(data, standard, cancer_type, config = NULL) {
  # Trust level
  trust_df <- tumour_benchmark_table(
    data = data,
    standard = standard,
    cancer_type = cancer_type,
    level = "trust",
    config = config
  )
  national_provider_count <- attr(trust_df, "national_provider_count")

  # National level
  national_df <- standard_compliance_calc(
    data = data,
    standard = standard,
    cancer_type = cancer_type,
    level = "national"
  ) |> add_compliance_rank()

  # Alliance level
  alliance_df <- standard_compliance_calc(
    data = data,
    standard = standard,
    cancer_type = cancer_type,
    level = "alliance"
  ) |> add_compliance_rank()

  # Combine
  result <- add_national_alliance_rows(
    trust_df = trust_df,
    national_df = national_df,
    alliance_df = alliance_df
  )

  attr(result, "national_provider_count") <- national_provider_count

  result
}


# =============================================================================
# CORRELATION PROCESSING
# =============================================================================

#' Process FDS vs 62-day correlation data
#'
#' @param fds_correlation FDS correlation dataframes
#' @param correlation_62 62-day correlation dataframe
#' @return List with correlation dataframes
process_fds_62_correlation <- function(fds_correlation, correlation_62) {
  if (is.null(fds_correlation) || is.null(correlation_62)) {
    return(NULL)
  }

  # rule_out/rule_in are NULL for a tumour with no detailed data
  # (cwt_fds_detailed null — public-sourced; see process_fds_correlation()) —
  # nothing to merge in that case.
  list(
    fds_vs_62 = merge(
      fds_correlation$overall,
      correlation_62,
      by = c("PERIOD", "ORG.NAME")
    ),
    fds_ro_vs_62 = if (is.null(fds_correlation$rule_out)) {
      NULL
    } else {
      merge(
        fds_correlation$rule_out,
        correlation_62,
        by.x = c("Date", "ORG.NAME"),
        by.y = c("PERIOD", "ORG.NAME")
      )
    },
    fds_ri_vs_62 = if (is.null(fds_correlation$rule_in)) {
      NULL
    } else {
      merge(
        fds_correlation$rule_in,
        correlation_62,
        by.x = c("Date", "ORG.NAME"),
        by.y = c("PERIOD", "ORG.NAME")
      )
    }
  )
}


# =============================================================================
# LWBC PROCESSING
# =============================================================================

#' Filter the five LWBC dataframes to one raw `Tumour Group` value
#'
#' @param global_data List containing all global dataframes
#' @param group_val Character(1). Raw `Tumour Group` value to filter on.
#' @param label Character. Used only in validate_df_not_empty_warn()'s message.
#' @return List with LWBC dataframes for this one group
process_lwbc_group <- function(global_data, group_val, label) {
  plot_df <- global_data$tumour_pack_lwbc_plot_df %>%
    filter(`Tumour Group` %in% group_val)
  plot_df <- validate_df_not_empty_warn(
    plot_df,
    paste("LWBC plot data for", label)
  )

  pa_plot_df <- global_data$tumour_pack_lwbc_pa_plot_df %>%
    filter(`Tumour Group` %in% group_val)
  pa_plot_df <- validate_df_not_empty_warn(
    pa_plot_df,
    paste("LWBC PA plot data for", label)
  )

  trust_plot_df <- global_data$tumour_pack_trust_lwbc_plot_df %>%
    filter(`Tumour Group` %in% group_val)
  trust_plot_df <- validate_df_not_empty_warn(
    trust_plot_df,
    paste("LWBC trust plot data for", label)
  )

  trust_pa_plot_df <- global_data$tumour_pack_trust_lwbc_pa_plot_df %>%
    filter(`Tumour Group` %in% group_val)
  trust_pa_plot_df <- validate_df_not_empty_warn(
    trust_pa_plot_df,
    paste("LWBC trust PA plot data for", label)
  )

  table_df <- global_data$tumour_pack_alliance_trust_lwbc_table_df %>%
    filter(`Tumour Group` %in% group_val)
  table_df <- validate_df_not_empty_warn(
    table_df,
    paste("LWBC table data for", label)
  )

  list(
    plot = plot_df,
    pa_plot = pa_plot_df,
    trust_plot = trust_plot_df,
    trust_pa_plot = trust_pa_plot_df,
    table = table_df
  )
}


#' Process LWBC data
#'
#' Gated on config$lwbc being non-null (previously this ran unconditionally
#' for every tumour, relying on a separate Contents-slide flag — has_lwbc —
#' to stay in step by hand). When config$lwbc is split (see the null / scalar
#' / array convention in config/tumours.yml's header), returns one set of the
#' five LWBC dataframes PER group, as a named list keyed by the raw group
#' value, mirroring process_staging_with_subtypes().
#'
#' @param global_data List containing all global dataframes
#' @param config Tumour configuration
#' @return List with LWBC dataframes (single group), or a named list of such
#'   lists keyed by raw group value (split), or NULL if this tumour has no
#'   LWBC data
process_lwbc <- function(global_data, config) {
  if (is.null(config$lwbc)) {
    return(NULL)
  }

  if (has_split(config, "lwbc")) {
    result <- list()
    for (group_val in config$lwbc) {
      result[[group_val]] <- process_lwbc_group(
        global_data, group_val, paste0(config$name, " (", group_val, ")")
      )
    }
    return(result)
  }

  process_lwbc_group(global_data, config$lwbc, config$name)
}


# =============================================================================
# TREATMENT VARIATION PROCESSING
# =============================================================================

#' Process treatment variation data
#'
#' Reshapes the long-format `tv_long` dataframe (and the indicator-text
#' lookup `tv_indicators`) into the two-item list consumed by the
#' Treatment Variation slides in the .Rmd template.
#'
#' @param global_data List containing `tv_long` and `tv_indicators`.
#' @param config Tumour configuration.
#' @return List with elements `overview` and `tumour`, or NULL when
#'   the tumour is not configured for treatment variation.
process_treatment_variation <- function(global_data, config) {
  if (is.null(config$treatment_variation)) {
    return(NULL)
  }

  tumour_name <- config$name
  tv_filter <- config$treatment_variation

  tv_long <- global_data$tv_long
  tv_indicators <- global_data$tv_indicators

  if (is.null(tv_long) || nrow(tv_long) == 0) {
    warning("Treatment variation data 'tv_long' missing or empty for ", tumour_name, call. = FALSE)
    return(NULL)
  }
  if (is.null(tv_indicators) || nrow(tv_indicators) == 0) {
    warning("Treatment variation indicators 'tv_indicators' missing or empty for ", tumour_name, call. = FALSE)
    return(NULL)
  }

  tumour_long <- tv_long |>
    filter(`Tumour Site` == tv_filter)

  if (nrow(tumour_long) == 0) {
    warning("No treatment variation rows for tumour '", tumour_name,
      "' (filter: '", tv_filter, "')",
      call. = FALSE
    )
    return(NULL)
  }

  indicator_row <- tv_indicators |>
    filter(`Tumour Site` == tv_filter)

  if (nrow(indicator_row) == 0) {
    warning("No indicator string for tumour '", tumour_name, "'", call. = FALSE)
    return(NULL)
  }

  indicator_text <- as.character(indicator_row[["Recommendation / Indicator"]][1])
  display_period <- as.character(indicator_row[["Time Period"]][1])

  alliance_name <- get_tv_config("alliance_name")
  tv_trust_names <- unname(get_source_name_map("tv_geography_map"))
  trust_to_icb <- get_trust_to_icb()

  national_label <- "National Moving Avg. (%)"
  alliance_label <- "Alliance Moving Avg. (%)"

  # ---- overview$table: latest period across alliance trusts + National + Alliance
  latest_key <- max(tumour_long$`Sort Key`, na.rm = TRUE)

  latest_rows <- tumour_long |>
    filter(`Sort Key` == latest_key) |>
    filter(
      `Provider name` %in% tv_trust_names |
        `Provider name` == "England" |
        `Provider name` == alliance_name
    ) |>
    mutate(
      Trusts = case_when(
        `Provider name` == "England" ~ national_label,
        `Provider name` == alliance_name ~ alliance_label,
        TRUE ~ `Provider name`
      )
    ) |>
    select(Trusts, `Moving average (%)`, `Case volume`)

  if (needs_tv_percentage_conversion()) {
    latest_rows <- latest_rows |>
      mutate(`Moving average (%)` = 100 * as.numeric(`Moving average (%)`))
  }

  overview_table <- latest_rows |>
    mutate(
      ICB = case_when(
        Trusts == national_label ~ "National",
        Trusts == alliance_label ~ "Alliance",
        TRUE ~ trust_to_icb[Trusts]
      )
    ) |>
    mutate(
      ICB = factor(
        ICB,
        levels = c(
          "National",
          "Alliance",
          sort(unique(ICB[!(ICB %in% c("National", "Alliance"))]))
        )
      )
    ) |>
    arrange(ICB, desc(`Moving average (%)`)) |>
    relocate(ICB, .before = Trusts) |>
    group_by(ICB) |>
    mutate(icb_group = cur_group_id()) |>
    ungroup()

  overview <- list(
    cancer = tumour_name,
    header = display_period,
    note   = indicator_text,
    table  = overview_table
  )

  # ---- tumour$data + tumour_numeric_table: alliance trusts only
  trust_long <- tumour_long |>
    filter(`Provider name` %in% tv_trust_names) |>
    arrange(`Sort Key`) |>
    transmute(
      Trust = `Provider name`,
      Period = `Time period`,
      `Moving avg (%)` = `Moving average (%)`,
      `Case volume` = `Case volume`,
      `Sort Key`
    )

  latest_8_keys <- trust_long |>
    distinct(`Sort Key`) |>
    arrange(desc(`Sort Key`)) |>
    slice_head(n = 8) |>
    pull(`Sort Key`)


  trust_long <- trust_long |>
    filter(`Sort Key` %in% latest_8_keys)

  period_levels <- trust_long |>
    distinct(Period, `Sort Key`) |>
    arrange(`Sort Key`) |>
    pull(Period)

  trust_long <- trust_long |>
    select(-`Sort Key`) |>
    mutate(Period = factor(Period, levels = period_levels))

  if (needs_tv_percentage_conversion()) {
    trust_long <- trust_long |>
      mutate(`Moving avg (%)` = 100 * as.numeric(`Moving avg (%)`))
  }

  tumour_numeric_table <- trust_long |>
    pivot_wider(
      id_cols     = Trust,
      names_from  = Period,
      values_from = c(`Moving avg (%)`, `Case volume`),
      names_glue  = "{Period}_{.value}"
    ) |>
    as.data.frame()

  ordered_cols <- as.vector(rbind(
    paste0(period_levels, "_Moving avg (%)"),
    paste0(period_levels, "_Case volume")
  ))
  tumour_numeric_table <- tumour_numeric_table |>
    select(Trust, all_of(ordered_cols))

  tumour <- list(
    data                 = trust_long,
    indicator_text       = indicator_text,
    tumour_numeric_table = tumour_numeric_table
  )

  list(
    overview = overview,
    tumour   = tumour
  )
}

# =============================================================================
# STANDARDS TABLE PROCESSING
# =============================================================================

#' Create combined standards table (FDS, 31-day, 62-day)
#'
#' @param fds_table FDS table dataframe
#' @param table_31 31-day table dataframe
#' @param table_62 62-day table dataframe
#' @param config Tumour configuration (for provider group ordering)
#' @return Combined standards table
create_standards_table <- function(fds_table, table_31, table_62, config) {
  result <- fds_table

  if (!is.null(table_31)) {
    result <- full_join(result, table_31, by = "ORG.NAME")
  }

  if (!is.null(table_62)) {
    result <- full_join(result, table_62, by = "ORG.NAME")
  }

  result <- apply_provider_group_ordering(
    result, config,
    code_col = "ORG.NAME",
    sort_col = "Latest 62-Day Compliance (%)", sort_desc = TRUE
  )

  # any_of() rather than a plain rename(): the two joins above are conditional,
  # so for a tumour with no 31-day/62-day data (an FDS-only referral route such
  # as non-specific symptoms) those columns were never added to `result`, and a
  # plain rename() errors with "Can't rename columns that don't exist".
  # any_of() renames what is present and silently skips the rest.
  #
  # Safe against typos despite the tolerance: these three names are produced a
  # few lines up in process_fds_table()/process_31_day_table()/
  # process_62_day_table(), and the all-standards case is covered by tests, so
  # a misspelling here still fails there.
  result %>%
    rename(any_of(c(
      `No. patients above FDS target` = "Patients_Above_FDS_Target_Latest",
      `No. patients above 31 Day target` = "Patients_Above_31_Day_Target_Latest",
      `No. patients above 62 Day target` = "Patients_Above_62_Day_Target_Latest"
    )))
}
