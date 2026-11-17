# Tumour Configuration
# ====================
# This file loads tumour_config from config/tumours.yml and data_source_names
# from config/data_sources.yml, and exposes helper functions (get_tumour_config,
# get_name_in_dataset, get_tumour_keys, has_split, ...).
#
# All field definitions and per-tumour values live in config/tumours.yml; see
# the comments at the top of that file for field semantics and the null /
# scalar / array convention.
#
# To add a new tumour type:
#   1. Add an entry to config/tumours.yml, setting every field.
#   2. Run the data pack generation script. No new RMarkdown file is needed:
#      scripts/main.R loops get_tumour_keys() and renders the parameterised
#      scripts/Tumour_Data_Pack_Template.Rmd for each key.


# =============================================================================
# CONFIG LOADER
# =============================================================================
#
# yaml::read_yaml returns sequences as R lists. The post-processor below
# converts known character-vector fields back to atomic vectors so consumer
# code that expects `is.character()` still works:
#
#   - The 11 null/scalar/array fields (see NULL_SCALAR_ARRAY_FIELDS below):
#     a list (YAML array) is flattened to a character vector; a scalar string
#     is already character(1); NULL is left as NULL.
#   - cwt_provider_exclusions   list -> character vector
#   - cwt_provider_ordering     list of groups; each group's `codes` flattened

#' Dataset fields following the null / scalar / array convention
#'
#' See the header of config/tumours.yml for what each value means. Used by
#' has_split(), get_tumour_config()'s post-processing, and
#' validate_tumour_config()'s type checks.
NULL_SCALAR_ARRAY_FIELDS <- c(
  "cwt_fds_public", "cwt_fds_detailed",
  "cwt_31day_public", "cwt_31day_detailed",
  "cwt_62day_public", "cwt_62day_detailed", "cwt_62day_public_comm",
  "staging_completeness", "staging_demographics",
  "lwbc", "treatment_variation"
)

#' Every field required on every config/tumours.yml entry
#'
#' validate_tumour_config() rejects an entry missing any of these, and rejects
#' any entry with a field not in this list.
REQUIRED_TUMOUR_FIELDS <- c(
  "name",
  "cwt_fds_public", "cwt_fds_detailed",
  "cwt_31day_public", "cwt_31day_detailed",
  "cwt_62day_public", "cwt_62day_detailed", "cwt_62day_public_comm",
  "cwt_public_data_exists_for_benchmarking",
  "display_benchmarking_fds", "display_benchmarking_31day", "display_benchmarking_62day",
  "staging_completeness", "staging_demographics", "lwbc",
  "treatment_variation", "treatment_variation_target", "treatment_variation_target_name",
  "cwt_provider_exclusions", "cwt_provider_ordering"
)

.find_config_file <- function(filename) {
  candidates <- c(file.path("config", filename),
                  file.path("..", "config", filename),
                  file.path("..", "..", "config", filename))
  for (p in candidates) if (file.exists(p)) return(p)
  stop("Could not locate config/", filename, " from working dir: ", getwd())
}

.post_process_tumour <- function(cfg) {
  for (field in NULL_SCALAR_ARRAY_FIELDS) {
    if (is.list(cfg[[field]])) {
      cfg[[field]] <- unlist(cfg[[field]], use.names = FALSE)
    }
  }
  if (!is.null(cfg$cwt_provider_exclusions)) {
    cfg$cwt_provider_exclusions <- unlist(cfg$cwt_provider_exclusions,
                                          use.names = FALSE)
  }
  if (!is.null(cfg$cwt_provider_ordering)) {
    cfg$cwt_provider_ordering <- lapply(cfg$cwt_provider_ordering, function(g) {
      g$codes <- unlist(g$codes, use.names = FALSE)
      g
    })
  }
  cfg
}

tumour_config <- local({
  raw <- yaml::read_yaml(.find_config_file("tumours.yml"))
  lapply(raw, .post_process_tumour)
})

#' Display source name per dataset field, loaded from config/data_sources.yml
#'
#' One flat map, not tumour-specific: each dataset key names the same
#' underlying NHS/NDRS publication for every tumour.
data_source_names <- local({
  yaml::read_yaml(.find_config_file("data_sources.yml"))
})


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get tumour configuration by key
#'
#' @param tumour_key Character string identifying the tumour type
#' @return List containing tumour configuration
#' @examples
#' get_tumour_config("lung")
get_tumour_config <- function(tumour_key) {
  if (!tumour_key %in% names(tumour_config)) {
    stop(paste("Unknown tumour type:", tumour_key,
               "\nAvailable types:", paste(names(tumour_config), collapse = ", ")))
  }
  tumour_config[[tumour_key]]
}


#' Get the raw value(s) used to identify a tumour in a specific dataset
#'
#' Thin accessor over config[[field]], kept as a named function (rather than
#' inlined at ~15 call sites) for a single point of truth and because the
#' result must always be a character vector for %in%-based filtering — which
#' get_tumour_config()'s post-processing already guarantees, so this simply
#' returns the value as-is.
#'
#' There is no fallback chain any more (see config/tumours.yml's header):
#' every field is set explicitly, including when it duplicates another
#' field's or the tumour's own `name` value.
#'
#' @param config Tumour configuration list (from get_tumour_config)
#' @param field Character string identifying the dataset field (e.g.,
#'   "cwt_fds_detailed", "staging_demographics")
#' @return Character vector of raw value(s) for filtering, or NULL if this
#'   tumour has no data for that field.
get_name_in_dataset <- function(config, field) {
  config[[field]]
}


#' Get the display source name for a dataset field
#'
#' @param field Character string identifying the dataset field (e.g.,
#'   "cwt_fds_detailed", "staging_demographics")
#' @return Character string, e.g. "NHSE CWT (internal extract)"
get_data_source_name <- function(field) {
  data_source_names[[field]]
}


#' Check whether a dataset field is split for a tumour
#'
#' A field is split when its value is a character vector of more than one raw
#' value (see the null / scalar / array convention in config/tumours.yml's
#' header): one sub-section per element, in the given order. A single string
#' (including NULL) is not split.
#'
#' @param config Tumour configuration list (from get_tumour_config)
#' @param field Character string identifying one of NULL_SCALAR_ARRAY_FIELDS
#' @return Logical
has_split <- function(config, field) {
  is.character(config[[field]]) && length(config[[field]]) > 1
}


#' Check whether a benchmarking standard should be displayed for a tumour
#'
#' Benchmarking slides are gated by two separate things, deliberately kept apart:
#'
#'   1. cwt_public_data_exists_for_benchmarking — does the public workbook
#'      have any bucket for this tumour at all. A data availability fact.
#'   2. display_benchmarking_<standard> — do we WANT that standard's slide in
#'      this tumour's pack. An editorial choice.
#'
#' This function answers (2). Callers must still check (1). The two are
#' different questions and conflating them makes it impossible to say "the
#' data exists but we don't want the slide", or to distinguish that from a
#' data gap.
#'
#' Unlike the old include_benchmarking_* fields, display_benchmarking_* is
#' always explicitly set (true or false) on every tumour — there is no
#' omitted-means-included default under the new schema.
#'
#' @param config Tumour configuration list (from get_tumour_config)
#' @param standard Character: "fds", "31day" or "62day"
#' @return Logical
includes_benchmarking <- function(config, standard) {
  key <- paste0("display_benchmarking_", standard)
  isTRUE(config[[key]])
}


#' Get all tumour keys
#'
#' @return Character vector of tumour keys
get_tumour_keys <- function() {
  names(tumour_config)
}


#' Validate tumour configuration
#'
#' @param tumour_key Character string identifying the tumour type
#' @return TRUE if valid, stops with error message if invalid
validate_tumour_config <- function(tumour_key) {
  raw <- tumour_config[[tumour_key]]
  if (is.null(raw)) {
    stop(paste("Unknown tumour type:", tumour_key,
               "\nAvailable types:", paste(names(tumour_config), collapse = ", ")))
  }

  missing <- setdiff(REQUIRED_TUMOUR_FIELDS, names(raw))
  if (length(missing) > 0) {
    stop(paste("Missing required fields for", tumour_key, ":",
               paste(missing, collapse = ", ")))
  }

  unknown <- setdiff(names(raw), REQUIRED_TUMOUR_FIELDS)
  if (length(unknown) > 0) {
    stop(paste("Unrecognised fields for", tumour_key, ":",
               paste(unknown, collapse = ", ")))
  }

  if (!is.character(raw$name) || length(raw$name) != 1) {
    stop(paste("name for", tumour_key, "must be a single character string"))
  }

  for (field in NULL_SCALAR_ARRAY_FIELDS) {
    val <- raw[[field]]
    if (!is.null(val) && !is.character(val)) {
      stop(paste0(field, " for ", tumour_key, " must be null, a string, or an array of strings"))
    }
  }

  for (field in c("cwt_public_data_exists_for_benchmarking",
                  "display_benchmarking_fds", "display_benchmarking_31day",
                  "display_benchmarking_62day")) {
    if (!is.logical(raw[[field]]) || length(raw[[field]]) != 1 || is.na(raw[[field]])) {
      stop(paste0(field, " for ", tumour_key, " must be true or false"))
    }
  }

  if (!isTRUE(raw$cwt_public_data_exists_for_benchmarking)) {
    for (field in c("cwt_fds_public", "cwt_31day_public", "cwt_62day_public")) {
      if (!is.null(raw[[field]])) {
        stop(paste0(field, " for ", tumour_key,
                     " must be null when cwt_public_data_exists_for_benchmarking is false"))
      }
    }
  }

  # Every standard needs at least one source for its own pack figures: a null
  # cwt_*_detailed means the detailed extract has no distinct bucket for this
  # tumour, and the pack figures come from cwt_*_public instead
  # (process_*_raw(), R/tumour_processing.R) — at least one of the pair must
  # be set, or that standard has no data to build its figures from at all.
  for (standard in c("fds", "31day", "62day")) {
    detailed_field <- paste0("cwt_", standard, "_detailed")
    public_field <- paste0("cwt_", standard, "_public")
    if (is.null(raw[[detailed_field]]) && is.null(raw[[public_field]])) {
      stop(paste0(detailed_field, " and ", public_field, " for ", tumour_key,
                   " cannot both be null — at least one source is required"))
    }
  }

  # cwt_62day_public_comm feeds the 62-Day Summary slide's commissioner-level
  # text specifically when 62-day is public-sourced (cwt_62day_detailed null);
  # when 62-day is detailed-sourced, that text is derived from the detailed
  # extract instead (derive_62_day_comm_from_detailed(),
  # R/tumour_processing.R), so this field has no role and should stay null.
  if (is.null(raw$cwt_62day_detailed) && is.null(raw$cwt_62day_public_comm)) {
    stop(paste("cwt_62day_public_comm for", tumour_key,
               "must be set when cwt_62day_detailed is null (62-day is public-sourced)"))
  }
  if (!is.null(raw$cwt_62day_detailed) && !is.null(raw$cwt_62day_public_comm)) {
    stop(paste("cwt_62day_public_comm for", tumour_key,
               "must be null when cwt_62day_detailed is set (62-day is detailed-sourced)"))
  }

  # treatment_variation, its target and its target name are kept in lockstep:
  # either all three are set, or all three are null. A half-set record is a
  # config mistake, not a valid state (slides without a target render empty,
  # or a target with no data to show it against).
  tv_fields <- c("treatment_variation", "treatment_variation_target",
                 "treatment_variation_target_name")
  tv_null <- vapply(tv_fields, function(f) is.null(raw[[f]]), logical(1))
  if (length(unique(tv_null)) != 1) {
    stop(paste("treatment_variation, treatment_variation_target and",
               "treatment_variation_target_name for", tumour_key,
               "must be either all null or all set"))
  }
  if (!is.null(raw$treatment_variation_target) &&
      (!is.numeric(raw$treatment_variation_target) || length(raw$treatment_variation_target) != 1)) {
    stop(paste("treatment_variation_target for", tumour_key, "must be a single number"))
  }
  if (!is.null(raw$treatment_variation_target_name) &&
      (!is.character(raw$treatment_variation_target_name) || length(raw$treatment_variation_target_name) != 1)) {
    stop(paste("treatment_variation_target_name for", tumour_key, "must be a single character string"))
  }

  if (!is.null(raw$cwt_provider_exclusions) && !is.character(raw$cwt_provider_exclusions)) {
    stop(paste("cwt_provider_exclusions for", tumour_key, "must be null or a character vector"))
  }

  if (!is.null(raw$cwt_provider_ordering)) {
    if (!is.list(raw$cwt_provider_ordering)) {
      stop(paste("cwt_provider_ordering for", tumour_key, "must be null or a list of groups"))
    }
    for (g in raw$cwt_provider_ordering) {
      if (is.null(g$codes) || !is.character(g$codes)) {
        stop(paste("cwt_provider_ordering for", tumour_key, "has a group with no character `codes`"))
      }
    }
  }

  TRUE
}


#' Validate all tumour configurations
#'
#' @return TRUE if all valid, stops with error message if any invalid
validate_all_configs <- function() {
  for (key in get_tumour_keys()) {
    validate_tumour_config(key)
  }
  message("All tumour configurations validated successfully.")
  TRUE
}


#' Print summary of tumour configuration
#'
#' @param tumour_key Character string identifying the tumour type
print_tumour_summary <- function(tumour_key) {
  config <- get_tumour_config(tumour_key)

  cat("=== Tumour Configuration:", config$name, "===\n")
  cat("Tumour key:", tumour_key, "\n")
  cat("Has staging completeness:", !is.null(config$staging_completeness), "\n")
  cat("Has staging demographics:", !is.null(config$staging_demographics), "\n")
  cat("Public data exists for benchmarking:", config$cwt_public_data_exists_for_benchmarking, "\n")

  for (field in NULL_SCALAR_ARRAY_FIELDS) {
    if (has_split(config, field)) {
      cat("Split (", field, "):", paste(config[[field]], collapse = ", "), "\n")
    }
  }

  if (!is.null(config$treatment_variation_target)) {
    cat("Treatment variation target:", config$treatment_variation_target * 100, "%\n")
  }

  cat("\n")
}


# =============================================================================
# SLIDE SECTION HELPER FUNCTIONS (for parameterised Rmd template)
# =============================================================================

#' Check if tumour has treatment variation data
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Logical
has_treatment_variation <- function(tumour_key) {
  config <- get_tumour_config(tumour_key)
  !is.null(config$treatment_variation)
}


#' Get treatment variation target constant name for a tumour
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Character string of the global constant's name (see
#'   config/globals.yml), or NULL
get_tv_target_name <- function(tumour_key) {
  config <- get_tumour_config(tumour_key)
  config$treatment_variation_target_name
}


#' Format one subtitle line, or a label line plus indented continuation lines
#'
#' A section-divider subtitle is built as a character vector — one element
#' per PowerPoint paragraph (see add_section_subtitles(), R/pptx_section_subtitles.R)
#' — never a single string with embedded separators, so there are no
#' parentheses or pipe-delimiters cluttering the slide: each value gets its
#' own line.
#'
#' @param label "Source" or "Filter". Pluralised to "Sources"/"Filters"
#'   automatically when `values` has more than one element — the same
#'   length check that decides whether the line is split at all, so the
#'   wording always agrees with how many lines actually follow.
#' @param values Character vector. Length 1: one line, "<label>:\t<value>".
#'   Length > 1 and NAMED (e.g. c(FDS = "...", `31-day` = "...")): one line
#'   per element, "<label>s:\t<name>: <value>" then indented "\t<name>: <value>"
#'   lines — used for the three CWT standards, which can genuinely have
#'   different sources/filters. Length > 1 and UNNAMED: one line per element,
#'   with no name repeated — used for a split field (staging_demographics,
#'   staging_completeness, lwbc), where the raw value already doubles as its
#'   own display name (see the null / scalar / array convention in
#'   config/tumours.yml's header), so repeating it as a label would be
#'   redundant.
#'
#'   A literal tab character separates the label from the value on every
#'   line, single-value lines included (instead of space-padding), so every
#'   value — a one-line "Source:" as well as a multi-line "Filters:" block
#'   beneath it — lands in the same column regardless of "Source:" vs
#'   "Filter:"'s actual rendered width — these are NOT the same
#'   width in the template's proportional font despite being the same
#'   character count. add_section_subtitles() (R/pptx_section_subtitles.R)
#'   gives that tab character a fixed PowerPoint tab stop; without it, a
#'   bare "\t" falls back to PowerPoint's default 1-inch tab increments,
#'   which does not reliably land at the same column as the first line (its
#'   own pre-tab text is a different width again).
#' @return Character vector of one or more lines.
format_subtitle_lines <- function(label, values) {
  if (length(values) == 1) {
    return(paste0(label, ":\t", unname(values[1])))
  }

  label <- paste0(label, "s")
  display <- if (!is.null(names(values))) paste0(names(values), ": ", values) else values
  c(paste0(label, ":\t", display[1]), paste0("\t", display[-1]))
}


#' Format the Cancer Waiting Times section subtitle
#'
#' Shows the data source, then the filter value each standard's figures are
#' actually built from (process_fds_raw()/process_31_day_raw()/
#' process_62_day_raw(), R/tumour_processing.R). A standard is sourced from
#' the detailed extract when its cwt_*_detailed field is set, or from the
#' public extract instead when that field is null (see the null / scalar /
#' array convention in config/tumours.yml's header). The Source line is
#' collapsed to one shared line when all three standards agree (the common
#' case); when they genuinely differ (e.g. HPB sources FDS from detailed but
#' 31-day/62-day from public), each gets its own line, same as Filter always
#' does. Public BENCHMARKING values are deliberately not shown here even when
#' a standard's own figures happen to also be public-sourced — this subtitle
#' is about the section's headline figures, not the separate Benchmarking
#' slide.
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Character vector of subtitle lines, e.g.
#'   c("Source:\tNHSE CWT (internal extract)",
#'     "Filters:\tFDS: Suspected lung cancer", "\t31-day: Lung",
#'     "\t62-day: Lung")
format_cwt_filter_subtitle <- function(tumour_key) {
  config <- get_tumour_config(tumour_key)

  standard_value <- function(detailed_field, public_field) {
    field <- if (!is.null(config[[detailed_field]])) detailed_field else public_field
    list(
      value = paste(get_name_in_dataset(config, field), collapse = "; "),
      source = get_data_source_name(field)
    )
  }

  fds <- standard_value("cwt_fds_detailed", "cwt_fds_public")
  day_31 <- standard_value("cwt_31day_detailed", "cwt_31day_public")
  day_62 <- standard_value("cwt_62day_detailed", "cwt_62day_public")

  sources <- c(FDS = fds$source, `31-day` = day_31$source, `62-day` = day_62$source)
  source_lines <- format_subtitle_lines(
    "Source",
    if (length(unique(sources)) == 1) unname(sources[1]) else sources
  )

  filter_lines <- format_subtitle_lines(
    "Filter",
    c(FDS = fds$value, `31-day` = day_31$value, `62-day` = day_62$value)
  )

  c(source_lines, filter_lines)
}


#' Format a section subtitle for a field that may be split
#'
#' Used for the Staging Completeness, Early Staging, Living With & Beyond
#' Cancer and Treatment Variation section subtitles, any of which may be
#' split per config/tumours.yml's null / scalar / array convention. Mirrors
#' the row-splitting logic in build_category_mapping_table()
#' (R/tumour_category_mapping.R) for the same fields, so the subtitle agrees
#' with the Appendix's Dataset Category Mapping table.
#'
#' @param tumour_key Character string identifying the tumour type
#' @param field Character: one of NULL_SCALAR_ARRAY_FIELDS
#' @return Character vector of subtitle lines. When split, e.g.
#'   c("Source:\t...", "Filters:\tColon", "\tRectal"); when not split,
#'   e.g. c("Source:\t...", "Filter:\tLung").
format_split_aware_filter_subtitle <- function(tumour_key, field) {
  config <- get_tumour_config(tumour_key)
  source_line <- format_subtitle_lines("Source", get_data_source_name(field))

  filter_values <- if (has_split(config, field)) {
    config[[field]]
  } else {
    paste(get_name_in_dataset(config, field), collapse = "; ")
  }
  filter_lines <- format_subtitle_lines("Filter", filter_values)

  c(source_line, filter_lines)
}


#' Generate contents slide items based on tumour configuration
#'
#' Returns a character vector of contents items for the data pack,
#' dynamically including/excluding sections based on tumour config. A
#' section's presence is driven entirely by whether its data field is
#' non-null — no separate has_* flag to keep in step any more.
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Character vector of contents items
generate_contents_items <- function(tumour_key) {
  config <- get_tumour_config(tumour_key)

  items <- c()
  item_num <- 1

  # Cancer Waiting Times (always shown)
  items <- c(items, paste0("[", item_num, ". Cancer Waiting Times](#cancer-waiting-times)"))
  item_num <- item_num + 1

  # Treatment Variation (conditional)
  if (!is.null(config$treatment_variation)) {
    items <- c(items, paste0("[", item_num, ". Treatment Variation](#treatment-variation)"))
    item_num <- item_num + 1
  }

  # Staging Completeness (conditional)
  if (!is.null(config$staging_completeness)) {
    items <- c(items, paste0("[", item_num, ". Staging Completeness](#staging-completeness)"))
    item_num <- item_num + 1
  }

  # Inequalities - Early Staging (conditional on demographics data)
  if (!is.null(config$staging_demographics)) {
    items <- c(items, paste0("[", item_num, ". Early Staging](#early-staging)"))
    item_num <- item_num + 1
  }

  # Living With & Beyond Cancer (conditional)
  if (!is.null(config$lwbc)) {
    items <- c(items, paste0("[", item_num, ". Living With & Beyond Cancer](#living-with-and-beyond-cancer)"))
    item_num <- item_num + 1
  }

  # Future sections (always shown)
  items <- c(items, paste0(item_num, ". Coming soon: Survival & mortality"))
  item_num <- item_num + 1

  # Additional Info (always shown)
  items <- c(items, paste0("[", item_num, ". Data Sources / Resources](#further-information)"))

  items
}
