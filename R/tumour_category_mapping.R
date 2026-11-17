# Tumour -> Dataset Category Mapping
# ===================================
# Builds the "how this tumour type is matched to source data" appendix table
# shown at the end of every data pack. For each real dataset a tumour's data
# is filtered from, the table shows every raw category value present in that
# dataset and which value(s) this tumour's config (config/tumours.yml)
# actually selects, so the audience can see exactly which option was used to
# build the pack.
#
# One row = one real dataframe/column actually filtered — never a value
# merged across datasets. A field such as cwt_31day_public can be split (see
# config/tumours.yml's null / scalar / array convention), in which case it
# gets one row PER group instead of one combined row.
#
# Two functions:
#   get_dataset_category_values()   Tumour-independent: the full set of raw
#                                   values in each real dataframe. Call once.
#   build_category_mapping_table()  Per-tumour: joins one tumour's selections
#                                   against that shared value set.

# =============================================================================
# DATASET VALUE EXTRACTION
# =============================================================================

#' Get all distinct raw category values present in each source dataframe
#'
#' Reads the actual data (not tumours.yml) to answer "what values could a
#' tumour have been matched against here?". Column and dataframe choices
#' mirror where each is filtered in R/tumour_processing.R.
#'
#' Keys match config/tumours.yml's null/scalar/array dataset fields directly
#' (see NULL_SCALAR_ARRAY_FIELDS in R/load_tumour_config.R), so
#' build_category_mapping_table() can index straight into this with a field
#' name — no separate translation table.
#'
#' @param global_data List of source dataframes as built by
#'   create_global_data_list().
#' @return Named list keyed by dataset field, each element a sorted character
#'   vector of distinct raw values. An unavailable dataframe or column yields
#'   character(0), not an error, so a partial global_data (e.g. in a test)
#'   still returns a usable list.
#' @export
get_dataset_category_values <- function(global_data) {

  unique_col <- function(df, col) {
    if (is.null(df) || !col %in% names(df)) return(character(0))
    sort(unique(as.character(df[[col]])))
  }

  unique_public_standard <- function(standard) {
    df <- global_data$latest_public_2_months_df
    if (is.null(df) || !all(c("STANDARD", "CANCER.TYPE") %in% names(df))) {
      return(character(0))
    }
    sort(unique(as.character(df$CANCER.TYPE[df$STANDARD == standard])))
  }

  list(
    # National benchmarking extract (latest_public_2_months_df), read
    # whenever process_benchmarking() includes that standard.
    cwt_fds_public    = unique_public_standard("FDS"),
    cwt_31day_public  = unique_public_standard("31D"),
    cwt_62day_public  = unique_public_standard("62D"),

    # Public provider/commissioner extract — a DIFFERENT dataframe from the
    # benchmarking extract above, even though the same config field
    # (cwt_*_public) names the raw value used against both. Read only for a
    # standard whose cwt_*_detailed is null (public-sourced own figures —
    # see process_fds_raw()/process_31_day_raw()/process_62_day_raw()); kept
    # as its own key ("..._provider") so the Appendix table never shows one
    # dataframe's possible values under the other's row.
    cwt_fds_provider      = unique_col(global_data$fds_df, "CANCER.TYPE"),
    cwt_31day_provider    = unique_col(global_data$day_31_df, "CANCER.TYPE"),
    cwt_62day_provider    = unique_col(global_data$day_62_df, "CANCER.TYPE"),
    cwt_62day_public_comm = unique_col(global_data$cwt_62d_comm_df, "Cancer Type"),

    # Detailed extracts — read only for a standard whose cwt_*_detailed is
    # non-null; these drive the pack's own CWT chart figures for it.
    cwt_fds_detailed    = unique_col(global_data$cwt_d_fds_df, "Cancer.Report.Category"),
    cwt_31day_detailed  = unique_col(global_data$cwt_d_31_day_df, "CancerReportCategory"),
    cwt_62day_detailed  = unique_col(global_data$cwt_d_62_day_df, "CancerReportCategory"),

    staging_completeness  = unique_col(global_data$stage_completeness_df, "CANCER_GROUP_NM"),
    staging_demographics  = unique_col(global_data$stage_df, "Cancer group"),
    lwbc                  = unique_col(global_data$tumour_pack_lwbc_plot_df, "Tumour Group"),
    treatment_variation    = unique_col(global_data$tv_long, "Tumour Site")
  )
}


# =============================================================================
# PER-TUMOUR MAPPING TABLE
# =============================================================================

#' Build the tumour-to-dataset category mapping table for one pack
#'
#' One row per real dataframe this tumour actually filters, applying the same
#' gating that dataframe's read uses in R/tumour_processing.R and
#' R/read_tumour_data.R, so a row only appears when that specific filter
#' operation genuinely runs for this tumour (e.g. a `cwt_fds_public` row only
#' when benchmarking is enabled for that standard). A split field (see
#' config/tumours.yml's null / scalar / array convention) gets one row PER
#' group, labelled with that group's own raw value.
#'
#' @param tumour_key Character. Tumour key (as used by get_tumour_config()).
#' @param dataset_values List from get_dataset_category_values().
#' @return Dataframe with columns Tumour, `Dataset field`,
#'   `Selected value(s)`, `All possible values`, one row per dataframe
#'   actually filtered.
#' @export
build_category_mapping_table <- function(tumour_key, dataset_values) {
  config <- get_tumour_config(tumour_key)
  rows <- list()

  row_for <- function(label, selected, field) {
    data.frame(
      Tumour = label,
      `Dataset field` = field,
      `Selected value(s)` = paste(selected, collapse = "; "),
      `All possible values` = paste(dataset_values[[field]], collapse = "; "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  add_row <- function(label, selected, field) {
    rows[[length(rows) + 1]] <<- row_for(label, selected, field)
  }

  # Adds either one combined row (not split) or one row per group (split),
  # labelling a split row with its own raw value (there is no separate
  # display name under the null / scalar / array convention).
  add_field_rows <- function(field) {
    if (is.null(config[[field]])) return(invisible(NULL))
    if (has_split(config, field)) {
      for (v in config[[field]]) add_row(v, v, field)
    } else {
      add_row(config$name, config[[field]], field)
    }
    invisible(NULL)
  }

  # Adds the row for one CWT standard's own pack figures: detailed when set,
  # else the public provider extract (a different dataframe/row from the
  # benchmarking row added separately below, even though both read the same
  # config field — see get_dataset_category_values()).
  add_standard_source_row <- function(detailed_field, public_field, provider_dataset_key) {
    if (!is.null(config[[detailed_field]])) {
      add_row(config$name, config[[detailed_field]], detailed_field)
    } else {
      add_row(config$name, config[[public_field]], provider_dataset_key)
    }
  }

  # FDS
  add_standard_source_row("cwt_fds_detailed", "cwt_fds_public", "cwt_fds_provider")
  if (isTRUE(config$cwt_public_data_exists_for_benchmarking) && includes_benchmarking(config, "fds")) {
    add_field_rows("cwt_fds_public")
  }

  # 31-day
  add_standard_source_row("cwt_31day_detailed", "cwt_31day_public", "cwt_31day_provider")
  if (isTRUE(config$cwt_public_data_exists_for_benchmarking) && includes_benchmarking(config, "31day")) {
    add_field_rows("cwt_31day_public")
  }

  # 62-day — public-sourced also brings its own commissioner-level row
  # (cwt_62day_public_comm), feeding the 62-Day Summary text.
  add_standard_source_row("cwt_62day_detailed", "cwt_62day_public", "cwt_62day_provider")
  if (is.null(config$cwt_62day_detailed)) {
    add_row(config$name, config$cwt_62day_public_comm, "cwt_62day_public_comm")
  }
  if (isTRUE(config$cwt_public_data_exists_for_benchmarking) && includes_benchmarking(config, "62day")) {
    add_field_rows("cwt_62day_public")
  }

  # Staging completeness, staging demographics, LWBC: each independently
  # null-gated and split-aware.
  add_field_rows("staging_completeness")
  add_field_rows("staging_demographics")
  add_field_rows("lwbc")

  # Treatment variation: null-gated; never split (one clinical audit match
  # per tumour).
  if (!is.null(config$treatment_variation)) {
    add_row(config$name, config$treatment_variation, "treatment_variation")
  }

  do.call(rbind, rows)
}
