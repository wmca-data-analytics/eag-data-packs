# Tests for load_tumour_config.R functions
# =============================================================================

# =============================================================================
# validate_tumour_config() / validate_all_configs()
# =============================================================================

test_that("validate_tumour_config() passes for all existing tumour types", {
  tumour_keys <- get_tumour_keys()

  expect_true(length(tumour_keys) > 0, "Should have at least one tumour type defined")

  for (key in tumour_keys) {
    expect_true(
      validate_tumour_config(key),
      info = paste("Tumour type", key, "should pass validation")
    )
  }
})

test_that("validate_tumour_config() rejects unknown tumour key", {
  expect_error(
    validate_tumour_config("nonexistent_tumour"),
    regexp = "Unknown tumour type: nonexistent_tumour"
  )
})

test_that("validate_tumour_config() catches a missing required field", {
  original_config <- tumour_config
  tumour_config$test_missing_field <<- create_mock_config("Test Tumour")
  tumour_config$test_missing_field$lwbc <<- NULL # drop a required field entirely

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_missing_field"),
    regexp = "Missing required fields for test_missing_field.*lwbc"
  )
})

test_that("validate_tumour_config() rejects an unrecognised field", {
  original_config <- tumour_config
  tumour_config$test_unknown_field <<- create_mock_config("Test Tumour")
  tumour_config$test_unknown_field$level <<- "tumour" # old-schema leftover key

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_unknown_field"),
    regexp = "Unrecognised fields for test_unknown_field.*level"
  )
})

test_that("validate_tumour_config() requires cwt_*_public to be null when benchmarking data doesn't exist", {
  original_config <- tumour_config
  tumour_config$test_bad_public <<- create_mock_config(
    "Test Tumour",
    cwt_public_data_exists_for_benchmarking = FALSE
  )
  tumour_config$test_bad_public$cwt_31day_public <<- "Test Tumour"

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_public"),
    regexp = "cwt_31day_public.*must be null when cwt_public_data_exists_for_benchmarking is false"
  )
})

test_that("validate_tumour_config() requires at least one of cwt_*_detailed/cwt_*_public per standard", {
  original_config <- tumour_config
  tumour_config$test_no_source <<- create_mock_config("Test Tumour", cwt_public_data_exists_for_benchmarking = FALSE)
  # `$<-`/`[[<-` with NULL REMOVES a list element rather than setting it to
  # NULL — `[<-` with a length-1 list containing NULL is the idiomatic way to
  # keep the key present with a NULL value, which is what we need to test
  # here (missing vs. present-but-null are different validation errors).
  tumour_config$test_no_source["cwt_31day_detailed"] <<- list(NULL)

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_no_source"),
    regexp = "cwt_31day_detailed and cwt_31day_public.*cannot both be null"
  )
})

test_that("validate_tumour_config() requires cwt_62day_public_comm exactly when 62-day is public-sourced", {
  original_config <- tumour_config

  # detailed set, comm also set — not allowed
  tumour_config$test_comm_extra <<- create_mock_config("Test Tumour")
  tumour_config$test_comm_extra$cwt_62day_public_comm <<- "Test Tumour"

  # detailed null, public set, comm missing — not allowed
  tumour_config$test_comm_missing <<- create_mock_config("Test Tumour")
  tumour_config$test_comm_missing["cwt_62day_detailed"] <<- list(NULL)

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_comm_extra"),
    regexp = "cwt_62day_public_comm.*must be null when cwt_62day_detailed is set"
  )
  expect_error(
    validate_tumour_config("test_comm_missing"),
    regexp = "cwt_62day_public_comm.*must be set when cwt_62day_detailed is null"
  )
})

test_that("validate_tumour_config() requires treatment_variation fields to be all-null or all-set", {
  original_config <- tumour_config
  tumour_config$test_half_tv <<- create_mock_config("Test Tumour")
  tumour_config$test_half_tv$treatment_variation <<- "Test Tumour"
  # target and target_name left NULL — half-set

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_half_tv"),
    regexp = "treatment_variation.*must be either all null or all set"
  )
})

test_that("validate_tumour_config() requires name to be a single character string", {
  original_config <- tumour_config
  tumour_config$test_bad_name <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_name$name <<- c("Test", "Tumour") # length 2, not 1

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_name"),
    regexp = "name for test_bad_name must be a single character string"
  )
})

test_that("validate_tumour_config() rejects a null/scalar/array field that isn't character", {
  original_config <- tumour_config
  tumour_config$test_bad_nsa <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_nsa$lwbc <<- 42 # numeric, not null/string/array of strings

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_nsa"),
    regexp = "lwbc for test_bad_nsa must be null, a string, or an array of strings"
  )
})

test_that("validate_tumour_config() requires the benchmarking display flags to be a single logical", {
  original_config <- tumour_config
  tumour_config$test_bad_logical <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_logical$display_benchmarking_fds <<- "yes" # not logical

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_logical"),
    regexp = "display_benchmarking_fds for test_bad_logical must be true or false"
  )
})

test_that("validate_tumour_config() requires treatment_variation_target to be a single number", {
  original_config <- tumour_config
  tumour_config$test_bad_tv_target <<- create_mock_config(
    "Test Tumour",
    treatment_variation = "Test Tumour"
  )
  tumour_config$test_bad_tv_target$treatment_variation_target <<- "0.5" # character, not numeric

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_tv_target"),
    regexp = "treatment_variation_target for test_bad_tv_target must be a single number"
  )
})

test_that("validate_tumour_config() requires treatment_variation_target_name to be a single character string", {
  original_config <- tumour_config
  tumour_config$test_bad_tv_target_name <<- create_mock_config(
    "Test Tumour",
    treatment_variation = "Test Tumour"
  )
  tumour_config$test_bad_tv_target_name$treatment_variation_target_name <<- 123 # numeric, not character

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_tv_target_name"),
    regexp = "treatment_variation_target_name for test_bad_tv_target_name must be a single character string"
  )
})

test_that("validate_tumour_config() requires cwt_provider_exclusions to be a character vector", {
  original_config <- tumour_config
  tumour_config$test_bad_exclusions <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_exclusions$cwt_provider_exclusions <<- 123 # numeric, not character

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_exclusions"),
    regexp = "cwt_provider_exclusions for test_bad_exclusions must be null or a character vector"
  )
})

test_that("validate_tumour_config() requires cwt_provider_ordering to be a list", {
  original_config <- tumour_config
  tumour_config$test_bad_ordering <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_ordering$cwt_provider_ordering <<- "not a list"

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_ordering"),
    regexp = "cwt_provider_ordering for test_bad_ordering must be null or a list of groups"
  )
})

test_that("validate_tumour_config() requires every cwt_provider_ordering group to have character codes", {
  original_config <- tumour_config
  tumour_config$test_bad_ordering_codes <<- create_mock_config("Test Tumour")
  tumour_config$test_bad_ordering_codes$cwt_provider_ordering <<- list(
    list(codes = NULL)
  )

  on.exit({
    tumour_config <<- original_config
  })

  expect_error(
    validate_tumour_config("test_bad_ordering_codes"),
    regexp = "cwt_provider_ordering for test_bad_ordering_codes has a group with no character .codes."
  )
})

test_that("validate_all_configs() validates all tumour configurations", {
  expect_message(
    result <- validate_all_configs(),
    regexp = "All tumour configurations validated successfully"
  )
  expect_true(result)
})


# =============================================================================
# get_tumour_config() / get_tumour_keys()
# =============================================================================

test_that("get_tumour_config() returns correct configuration", {
  lung_config <- get_tumour_config("lung")

  expect_equal(lung_config$name, "Lung")
  expect_equal(lung_config$staging_completeness, "Lung")
  expect_equal(lung_config$cwt_fds_public, "Suspected lung cancer")
  expect_null(get_tumour_config("brain_cns")$cwt_fds_public) # no benchmarking bucket exists
})

test_that("get_tumour_config() rejects unknown tumour key", {
  expect_error(
    get_tumour_config("unknown_type"),
    regexp = "Unknown tumour type: unknown_type"
  )
})

test_that("get_tumour_keys() returns every tumour entry", {
  keys <- get_tumour_keys()

  # Long-standing types. Deliberately a subset check, not an exact count:
  # tumours get added over time, and a hardcoded count makes every such
  # addition fail an unrelated test.
  expected_keys <- c(
    "lung", "breast", "head_neck", "skin", "brain_cns", "brain_cns_neurosurgery",
    "lower_gi", "gynae", "urology", "upper_gi", "nss", "sarcoma"
  )

  for (key in expected_keys) {
    expect_true(key %in% keys, info = paste(key, "should be in tumour_keys"))
  }

  # There is no separate sub-type entity any more — every entry IS a pack.
  expect_setequal(keys, names(tumour_config))
})


# =============================================================================
# Data availability (null / scalar / array convention)
# =============================================================================

test_that("null fields correctly signal no data", {
  # Lung: full staging data and a public CWT benchmarking bucket
  lung_config <- get_tumour_config("lung")
  expect_equal(lung_config$cwt_fds_public, "Suspected lung cancer")
  expect_false(is.null(lung_config$staging_completeness))
  expect_false(is.null(lung_config$staging_demographics))
  expect_false(has_split(lung_config, "staging_demographics"))

  # Brain & CNS: no staging data and no public benchmarking bucket at all
  brain_config <- get_tumour_config("brain_cns")
  expect_null(brain_config$cwt_fds_public)
  expect_false(brain_config$cwt_public_data_exists_for_benchmarking)
  expect_null(brain_config$staging_completeness)
  expect_null(brain_config$staging_demographics)

  brain_ns_config <- get_tumour_config("brain_cns_neurosurgery")
  expect_null(brain_ns_config$staging_completeness)
  expect_null(brain_ns_config$staging_demographics)
})

test_that("has_split() identifies split tumours correctly", {
  # Urology splits staging_demographics, staging_completeness AND the
  # 31-day/62-day public benchmarking filter — but not FDS or detailed.
  uro_config <- get_tumour_config("urology")
  expect_true(has_split(uro_config, "staging_demographics"))
  expect_true(has_split(uro_config, "staging_completeness"))
  expect_true(has_split(uro_config, "cwt_31day_public"))
  expect_true(has_split(uro_config, "cwt_62day_public"))
  expect_false(has_split(uro_config, "cwt_fds_public"))
  expect_false(has_split(uro_config, "cwt_31day_detailed"))
  expect_false(has_split(uro_config, "cwt_62day_detailed"))

  # Gynae/Lower GI/Upper GI/HPB split staging_demographics only
  for (key in c("gynae", "lower_gi", "upper_gi")) {
    cfg <- get_tumour_config(key)
    expect_true(has_split(cfg, "staging_demographics"), info = key)
    expect_false(has_split(cfg, "staging_completeness"), info = key)
  }

  # Lung has no splits at all
  expect_false(has_split(get_tumour_config("lung"), "staging_demographics"))
})

test_that("has_split() treats a single string, and NULL, as not split", {
  expect_false(has_split(list(x = "A"), "x"))
  expect_false(has_split(list(x = NULL), "x"))
  expect_true(has_split(list(x = c("A", "B")), "x"))
})


# =============================================================================
# get_name_in_dataset() / get_data_source_name()
# =============================================================================

test_that("get_name_in_dataset() returns the field's value directly, no fallback", {
  config <- get_tumour_config("lung")
  expect_equal(get_name_in_dataset(config, "cwt_fds_detailed"), "Suspected lung cancer")
  expect_equal(get_name_in_dataset(config, "lwbc"), "Lung")
  expect_null(get_name_in_dataset(get_tumour_config("brain_cns"), "cwt_fds_public"))
})

test_that("get_name_in_dataset() returns a split field's full vector", {
  config <- get_tumour_config("urology")
  day_31 <- get_name_in_dataset(config, "cwt_31day_public")
  expect_true(is.character(day_31))
  expect_equal(length(day_31), 2)
  expect_true("Urological - Other (a)" %in% day_31)
  expect_true("Urological - Prostate" %in% day_31)
})

test_that("get_data_source_name() returns the configured source string per field", {
  expect_equal(
    get_data_source_name("cwt_fds_detailed"),
    "NHSE CWT (internal extract)"
  )
  expect_equal(
    get_data_source_name("treatment_variation"),
    "NATCAN"
  )
})


# =============================================================================
# includes_benchmarking()
# =============================================================================

test_that("includes_benchmarking() reads the explicit display_benchmarking_* flag", {
  config <- get_tumour_config("lung")
  expect_true(includes_benchmarking(config, "fds"))
  expect_true(includes_benchmarking(config, "31day"))
  expect_true(includes_benchmarking(config, "62day"))
})

test_that("includes_benchmarking() excludes only standards explicitly set false", {
  config <- get_tumour_config("nss")
  expect_true(includes_benchmarking(config, "fds"))
  expect_false(includes_benchmarking(config, "31day"))
  expect_false(includes_benchmarking(config, "62day"))
})


# =============================================================================
# Slide section helper functions
# =============================================================================

test_that("has_treatment_variation() and get_tv_target_name() work correctly", {
  expect_true(has_treatment_variation("lung"))
  expect_equal(get_tv_target_name("lung"), "TV_LUNG_TARGET")

  expect_false(has_treatment_variation("head_neck"))
  expect_null(get_tv_target_name("head_neck"))
})

test_that("generate_contents_items() returns items based on non-null fields", {
  lung_items <- generate_contents_items("lung")
  expect_true(any(grepl("Cancer Waiting Times", lung_items)))
  expect_true(any(grepl("Treatment Variation", lung_items)))
  expect_true(any(grepl("Staging Completeness", lung_items)))
  expect_true(any(grepl("Early Staging", lung_items)))
  expect_true(any(grepl("Living With & Beyond Cancer", lung_items)))

  hn_items <- generate_contents_items("head_neck")
  expect_true(any(grepl("Cancer Waiting Times", hn_items)))
  expect_false(any(grepl("Treatment Variation", hn_items)))
  expect_true(any(grepl("Staging Completeness", hn_items)))
  expect_false(any(grepl("Early Staging", hn_items)))

  brain_items <- generate_contents_items("brain_cns")
  expect_true(any(grepl("Cancer Waiting Times", brain_items)))
  expect_false(any(grepl("Staging Completeness", brain_items)))
  expect_false(any(grepl("Early Staging", brain_items)))

  nss_items <- generate_contents_items("nss")
  expect_false(any(grepl("Living With & Beyond Cancer", nss_items)))
})


# =============================================================================
# Section subtitle helpers (title-slide filter strings)
# =============================================================================

test_that("format_subtitle_lines() formats a single value as one tab-separated line", {
  # Tab, not space, so a single-value "Source:" line's value aligns with the
  # tab-separated "Filters:" block beneath it.
  expect_equal(format_subtitle_lines("Source", "ABC"), "Source:\tABC")
})

test_that("format_subtitle_lines() formats named values as a pluralised label line plus tab-indented sub-label lines", {
  # A literal tab, not space-padding, separates the label from the value —
  # the template's proportional font renders "Filter:" and each standard
  # label ("FDS:", "31-day:", "62-day:") at different widths despite equal
  # character counts, so space-padding can't actually align the columns.
  expect_equal(
    format_subtitle_lines("Filter", c(FDS = "X", `31-day` = "Y", `62-day` = "Z")),
    c("Filters:\tFDS: X", "\t31-day: Y", "\t62-day: Z")
  )
})

test_that("format_subtitle_lines() formats unnamed multi-values as a pluralised label line plus tab-indented plain lines", {
  expect_equal(
    format_subtitle_lines("Filter", c("Colon", "Rectal")),
    c("Filters:\tColon", "\tRectal")
  )
})

test_that("format_subtitle_lines() does not pluralise a single-element named vector", {
  expect_equal(format_subtitle_lines("Source", c(FDS = "X")), "Source:\tX")
})

test_that("format_cwt_filter_subtitle() shows one Source line and three Filter lines when all standards share a source", {
  internal <- "NHSE CWT (internal extract)"
  expect_equal(
    format_cwt_filter_subtitle("lung"),
    c(
      paste0("Source:\t", internal),
      "Filters:\tFDS: Suspected lung cancer",
      "\t31-day: Lung",
      "\t62-day: Lung"
    )
  )
  # urology: all three detailed values genuinely differ from each other, but
  # all three are still detailed-sourced (one shared Source line)
  expect_equal(
    format_cwt_filter_subtitle("urology"),
    c(
      paste0("Source:\t", internal),
      "Filters:\tFDS: Suspected urological malignancies (excluding testicular)",
      "\t31-day: Urological",
      "\t62-day: Urological (Excluding Testicular)"
    )
  )
})

test_that("format_cwt_filter_subtitle() splits the Source line too when standards genuinely differ", {
  original_config <- tumour_config
  tumour_config$test_mixed_source <<- create_mock_config(
    "Test Tumour",
    cwt_31day_detailed = NULL, cwt_31day_public = "Test Tumour - Specific",
    cwt_62day_detailed = NULL, cwt_62day_public = "Test Tumour - Specific",
    cwt_62day_public_comm = "Test Tumour - Specific"
  )
  on.exit({
    tumour_config <<- original_config
  })

  result <- format_cwt_filter_subtitle("test_mixed_source")

  internal <- "NHSE CWT (internal extract)"
  public <- "NHSE CWT (public release)"
  expect_equal(
    result,
    c(
      paste0("Sources:\tFDS: ", internal),
      paste0("\t31-day: ", public),
      paste0("\t62-day: ", public),
      "Filters:\tFDS: Test Tumour",
      "\t31-day: Test Tumour - Specific",
      "\t62-day: Test Tumour - Specific"
    )
  )
})

test_that("format_split_aware_filter_subtitle() shows one Source line and one Filter line when not split", {
  expect_equal(
    format_split_aware_filter_subtitle("skin", "staging_demographics"),
    c("Source:\tNDRS RCRD", "Filter:\tMelanoma")
  )
  expect_equal(
    format_split_aware_filter_subtitle("lower_gi", "staging_completeness"),
    c("Source:\tNDRS RCRD", "Filter:\tLower GI")
  )
})

test_that("format_split_aware_filter_subtitle() lists each group's value on its own line when split, with a pluralised Filters label", {
  expect_equal(
    format_split_aware_filter_subtitle("lower_gi", "staging_demographics"),
    c("Source:\tNDRS RCRD", "Filters:\tColon", "\tRectal")
  )
  expect_equal(
    format_split_aware_filter_subtitle("urology", "staging_completeness"),
    c("Source:\tNDRS RCRD", "Filters:\tUrological", "\tProstate")
  )
})

test_that("format_split_aware_filter_subtitle() handles treatment_variation and lwbc", {
  expect_equal(
    format_split_aware_filter_subtitle("lung", "treatment_variation"),
    c("Source:\tNATCAN", "Filter:\tLung")
  )
  expect_equal(
    format_split_aware_filter_subtitle("lung", "lwbc"),
    c("Source:\tNDRS COSD", "Filter:\tLung")
  )
})

# =============================================================================
# .find_config_file()
# =============================================================================
# Mirrors the equivalent tests for .find_config_dir() in test-load_globals.R
# (R/load_globals.R and R/load_alliance_config.R's own copy of this helper).

test_that(".find_config_file() finds tumours.yml from the project root", {
  withr::local_dir(project_root)
  expect_equal(.find_config_file("tumours.yml"), file.path("config", "tumours.yml"))
})

test_that(".find_config_file() finds tumours.yml from one level down", {
  withr::local_dir(file.path(project_root, "R"))
  expect_equal(.find_config_file("tumours.yml"), file.path("..", "config", "tumours.yml"))
})

test_that(".find_config_file() finds tumours.yml from two levels down", {
  withr::local_dir(file.path(project_root, "tests", "testthat"))
  expect_equal(.find_config_file("tumours.yml"), file.path("..", "..", "config", "tumours.yml"))
})

test_that(".find_config_file() errors when the file isn't reachable", {
  withr::local_dir(tempdir())
  expect_error(.find_config_file("tumours.yml"), "Could not locate config/tumours.yml")
})

# =============================================================================
# .post_process_tumour()
# =============================================================================
# yaml::read_yaml already parses a plain string sequence as a character
# vector, so none of these is.list() branches fire for the real
# config/tumours.yml (confirmed by inspecting the raw parse directly) - they
# only matter for a YAML shape read_yaml happens to hand back as a list
# instead, which these tests construct directly.

test_that(".post_process_tumour() unlists a null/scalar/array field that came back as a list", {
  cfg <- list(name = "Test", lwbc = list("Group A", "Group B"))

  result <- .post_process_tumour(cfg)

  expect_type(result$lwbc, "character")
  expect_equal(result$lwbc, c("Group A", "Group B"))
})

test_that(".post_process_tumour() leaves a null/scalar/array field alone when it's already a scalar", {
  cfg <- list(name = "Test", lwbc = "Lung")

  result <- .post_process_tumour(cfg)

  expect_equal(result$lwbc, "Lung")
})

test_that(".post_process_tumour() unlists cwt_provider_exclusions when it comes back as a list", {
  cfg <- list(name = "Test", cwt_provider_exclusions = list("RRK", "RKB"))

  result <- .post_process_tumour(cfg)

  expect_type(result$cwt_provider_exclusions, "character")
  expect_equal(result$cwt_provider_exclusions, c("RRK", "RKB"))
})

test_that(".post_process_tumour() unlists each cwt_provider_ordering group's codes", {
  cfg <- list(
    name = "Test",
    cwt_provider_ordering = list(
      list(name = "Treatment", codes = list("RRK", "RKB")),
      list(name = "Diagnostic", codes = list("RL4"))
    )
  )

  result <- .post_process_tumour(cfg)

  expect_type(result$cwt_provider_ordering[[1]]$codes, "character")
  expect_equal(result$cwt_provider_ordering[[1]]$codes, c("RRK", "RKB"))
  expect_equal(result$cwt_provider_ordering[[2]]$codes, "RL4")
})

# =============================================================================
# print_tumour_summary()
# =============================================================================

test_that("print_tumour_summary() reports a split field", {
  out <- capture.output(print_tumour_summary("urology"))

  expect_true(any(grepl("Tumour Configuration: Urology", out)))
  expect_true(any(grepl("Split \\( staging_completeness \\): Urological, Prostate", out)))
})

test_that("print_tumour_summary() reports the treatment variation target as a percentage", {
  out <- capture.output(print_tumour_summary("lung"))

  expect_true(any(grepl("Treatment variation target: 70 %", out)))
})
