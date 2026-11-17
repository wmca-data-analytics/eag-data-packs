# Fixture Factory for tumour_processing.R Tests
# =============================================================================
# These helper functions create realistic test dataframes for testing the
# processing functions in tumour_processing.R.

# -----------------------------------------------------------------------------
# Common test data
# -----------------------------------------------------------------------------

# Default test trusts (subset of WMCA)
DEFAULT_TRUSTS <- c("RRK", "RKB", "RL4")
DEFAULT_TRUST_NAMES <- c(
  "RRK" = "University Hospitals Birmingham",
  "RKB" = "University Hospitals Coventry & Warwickshire",
  "RL4" = "Royal Wolverhamption"
)

# Default test ICBs (subset of WMCA, codes as per icb_recode_map in alliances.yml)
DEFAULT_ICBS <- c("15E", "D2P2L", "B2M3M")
DEFAULT_ICB_NAMES <- c(
  "15E" = "Birmingham and Solihull",
  "D2P2L" = "Black Country",
  "B2M3M" = "Coventry and Warwickshire"
)

# Default test periods
DEFAULT_PERIODS <- as.Date(c("2024-04-01", "2024-05-01", "2024-06-01"))

# -----------------------------------------------------------------------------
# FDS Fixture Creators
# -----------------------------------------------------------------------------

#' Create FDS provider fixture (public CWT data)
#'
#' Creates a dataframe matching the structure of fds_df used by process_fds_raw()
#'
#' @param cancer_type Character. Cancer type value (e.g., "Lung")
#' @param trusts Character vector. Trust codes to include
#' @param periods Date vector. Periods to include
#' @param compliance_range Numeric vector of length 2. Range for random compliance
#' @return Dataframe with FDS provider structure
create_fds_provider_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS,
  compliance_range = c(70, 95)
) {
  # Create all combinations
  df <- expand.grid(
    CANCER.TYPE = cancer_type,
    ORG.CODE = trusts,
    PERIOD = periods,
    stringsAsFactors = FALSE
  )

  # Add trust names and metrics
  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- 100
  df$WITHIN.STANDARD <- sample(
    compliance_range[1]:compliance_range[2],
    nrow(df),
    replace = TRUE
  )

  df
}


#' Create FDS commissioner fixture
#'
#' Creates a dataframe matching the structure of cwt_fds_comm_df
#'
#' @param cancer_type Character. Cancer type value
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Periods
#' @return Dataframe with FDS commissioner structure
create_fds_comm_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    `Cancer Type` = cancer_type,
    ORG.CODE = trusts,
    PERIOD = periods,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- 100
  df$WITHIN.STANDARD <- sample(70:95, nrow(df), replace = TRUE)

  # Fix column name (expand.grid mangles it)
  names(df)[1] <- "Cancer Type"

  df
}


#' Create FDS detailed fixture
#'
#' Creates a dataframe matching the structure of cwt_d_fds_df
#'
#' @param cancer_type Character. Cancer.Report.Category value
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Dates
#' @param pathway_reasons Character vector. Pathway end reasons to include
#' @return Dataframe with FDS detailed structure
create_fds_detailed_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS,
  pathway_reasons = c("Diagnosis of cancer", "Ruling out of cancer")
) {
  df <- expand.grid(
    Cancer.Report.Category = cancer_type,
    Provider.Code.FDS = trusts,
    Date = periods,
    Pathway.End.Reason = pathway_reasons,
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$Provider.Code.FDS]
  df$NoPatients <- sample(20:50, nrow(df), replace = TRUE)

  # Distribute patients across day buckets (sum should roughly equal 80% for compliance)
  df$DaysWithin7 <- round(df$NoPatients * 0.3)
  df$Days8To14 <- round(df$NoPatients * 0.25)
  df$Days15To21 <- round(df$NoPatients * 0.15)
  df$Days22To28 <- round(df$NoPatients * 0.1)

  df
}


# -----------------------------------------------------------------------------
# 31-Day Fixture Creators
# -----------------------------------------------------------------------------

#' Create 31-day public fixture
#'
#' Creates a dataframe matching the structure of day_31_df
#'
#' @param cancer_type Character or vector. Cancer type(s)
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Periods
#' @return Dataframe with 31-day structure
create_31_day_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CANCER.TYPE = cancer_type,
    ORG.CODE = trusts,
    PERIOD = periods,
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- sample(80:120, nrow(df), replace = TRUE)
  df$WITHIN.STANDARD <- round(df$TOTAL.TREATED * runif(nrow(df), 0.90, 0.99))

  df
}


#' Create 31-day detailed fixture
#'
#' Creates a dataframe matching the structure of cwt_d_31_day_df
#'
#' @param cancer_type Character. CancerReportCategory value
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Dates
#' @return Dataframe with 31-day detailed structure
create_31_day_detailed_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CancerReportCategory = cancer_type,
    ProviderCode = trusts,
    Date = periods,
    AdmittedCare = c("Admitted", "Non-admitted"),
    TreatmentModality = "Surgery",
    stringsAsFactors = FALSE
  )

  df$Trust_Name <- DEFAULT_TRUST_NAMES[df$ProviderCode]
  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ProviderCode]
  df$NoTreated <- sample(20:50, nrow(df), replace = TRUE)
  df$DaysWithin31 <- round(df$NoTreated * runif(nrow(df), 0.90, 0.99))

  df
}


#' Create 31-day detailed commissioner fixture
#'
#' Creates a dataframe matching the structure of cwt_d_31_day_comm_df, which is
#' keyed on CCG (ICB) codes rather than provider codes.
#'
#' @param cancer_type Character. CancerReportCategory value
#' @param icbs Character vector. CCG/ICB codes
#' @param periods Date vector. Dates
#' @return Dataframe with 31-day detailed commissioner structure
create_31_day_detailed_comm_fixture <- function(
  cancer_type = "Lung",
  icbs = DEFAULT_ICBS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CancerReportCategory = cancer_type,
    CCG = icbs,
    Date = periods,
    AdmittedCare = c("Admitted", "Non-admitted"),
    TreatmentModality = "Surgery",
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_ICB_NAMES[df$CCG]
  df$NoTreated <- sample(20:50, nrow(df), replace = TRUE)
  df$DaysWithin31 <- round(df$NoTreated * runif(nrow(df), 0.90, 0.99))

  df
}


# -----------------------------------------------------------------------------
# 62-Day Fixture Creators
# -----------------------------------------------------------------------------

#' Create 62-day public fixture
#'
#' Creates a dataframe matching the structure of day_62_df
#'
#' @param cancer_type Character or vector. Cancer type(s)
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Periods
#' @return Dataframe with 62-day structure
create_62_day_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CANCER.TYPE = cancer_type,
    ORG.CODE = trusts,
    PERIOD = periods,
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  # 62-day uses half-patients
  df$TOTAL.TREATED <- sample(80:120, nrow(df), replace = TRUE) + sample(c(0, 0.5), nrow(df), replace = TRUE)
  df$WITHIN.STANDARD <- round(df$TOTAL.TREATED * runif(nrow(df), 0.80, 0.92) * 2) / 2

  df
}


#' Create 62-day commissioner fixture
#'
#' @param cancer_type Character. Cancer Type value
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Periods
#' @return Dataframe with 62-day commissioner structure
create_62_day_comm_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    `Cancer Type` = cancer_type,
    ORG.CODE = trusts,
    PERIOD = periods,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- sample(80:120, nrow(df), replace = TRUE)
  df$WITHIN.STANDARD <- round(df$TOTAL.TREATED * runif(nrow(df), 0.80, 0.92) * 2) / 2

  names(df)[1] <- "Cancer Type"

  df
}


#' Create 62-day detailed fixture
#'
#' @param cancer_type Character. CancerReportCategory value
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Dates
#' @return Dataframe with 62-day detailed structure
create_62_day_detailed_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CancerReportCategory = cancer_type,
    AccountableProvider = trusts,
    Date = periods,
    AdmittedCare = "Admitted",
    TreatmentModality = "Surgery",
    stringsAsFactors = FALSE
  )

  df$Trust_Name <- DEFAULT_TRUST_NAMES[df$AccountableProvider]
  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$AccountableProvider]
  df$NoTreated <- sample(20:50, nrow(df), replace = TRUE)
  df$DaysWithin31 <- round(df$NoTreated * 0.4)
  df$Days32to38 <- round(df$NoTreated * 0.2)
  df$Days39to48 <- round(df$NoTreated * 0.15)
  df$Days49to62 <- round(df$NoTreated * 0.1)

  df
}


#' Create 62-day detailed commissioner fixture
#'
#' Creates a dataframe matching the structure of cwt_d_62_day_comm_df, which is
#' keyed on CCG (ICB) codes rather than provider codes. Consumed by
#' derive_62_day_comm_from_detailed() for tumours without public CWT data.
#'
#' @param cancer_type Character. CancerReportCategory value
#' @param icbs Character vector. CCG/ICB codes
#' @param periods Date vector. Dates
#' @return Dataframe with 62-day detailed commissioner structure
create_62_day_detailed_comm_fixture <- function(
  cancer_type = "Lung",
  icbs = DEFAULT_ICBS,
  periods = DEFAULT_PERIODS
) {
  df <- expand.grid(
    CancerReportCategory = cancer_type,
    CCG = icbs,
    Date = periods,
    AdmittedCare = "Admitted",
    TreatmentModality = "Surgery",
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_ICB_NAMES[df$CCG]
  df$NoTreated <- sample(20:50, nrow(df), replace = TRUE)
  df$DaysWithin31 <- round(df$NoTreated * 0.4)
  df$Days32to38 <- round(df$NoTreated * 0.2)
  df$Days39to48 <- round(df$NoTreated * 0.15)
  df$Days49to62 <- round(df$NoTreated * 0.1)

  df
}


# -----------------------------------------------------------------------------
# Staging Fixture Creators
# -----------------------------------------------------------------------------

#' Create staging completeness fixture
#'
#' @param cancer_group Character. CANCER_GROUP_NM value
#' @param trusts Character vector. Trust codes
#' @param quarters Date vector. Diagnosis quarters
#' @return Dataframe with staging completeness structure
create_staging_completeness_fixture <- function(
  cancer_group = "Lung",
  trusts = DEFAULT_TRUSTS,
  quarters = as.Date(c("2024-01-01", "2024-04-01", "2024-07-01"))
) {
  df <- expand.grid(
    CANCER_GROUP_NM = cancer_group,
    ORG.CODE = trusts,
    DIAGNOSIS_QUARTER = quarters,
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$MDT_DISCUSSED <- sample(50:100, nrow(df), replace = TRUE)
  df$MDT_STAGE <- round(df$MDT_DISCUSSED * runif(nrow(df), 0.85, 0.98))

  df
}


#' Create staging demographics fixture
#'
#' @param cancer_group Character. Cancer group value
#' @param geography_types Character vector. Geography types to include
#' @param periods Date vector. Dates
#' @return Dataframe with staging demographics structure
create_staging_demographics_fixture <- function(
  cancer_group = "Lung",
  geography_types = c("Cancer alliance", "Provider"),
  periods = as.Date(c("2024-10-01", "2025-01-01", "2025-04-01"))
) {
  breakdowns <- c("Total", "Gender", "Age-group", "Ethnicity", "Deprivation", "Route to Diagnosis")
  demographics <- c("Total", "Male", "Female", "65+", "Under 65")

  df <- expand.grid(
    `Cancer group` = cancer_group,
    `Geography type` = geography_types,
    Date = periods,
    Metric = "Early stage proportion",
    Breakdown = breakdowns,
    Demographic = demographics,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  # Add org name for Provider rows
  df$ORG.NAME <- ifelse(
    df$`Geography type` == "Provider",
    sample(DEFAULT_TRUST_NAMES, nrow(df), replace = TRUE),
    NA
  )

  df$`Statistic (12m)` <- runif(nrow(df), 0.4, 0.7)

  # Fix column names
  names(df)[1] <- "Cancer group"
  names(df)[2] <- "Geography type"
  names(df)[8] <- "Statistic (12m)"

  df
}


# -----------------------------------------------------------------------------
# Mock Config Creator
# -----------------------------------------------------------------------------
#' Create a mock tumour config for testing
#'
#' Creates a config object matching the new config/tumours.yml schema (see its
#' header for field semantics and the null / scalar / array convention). Every
#' REQUIRED_TUMOUR_FIELDS key is set, so the result passes
#' validate_tumour_config() unchanged — tests that need an invalid config
#' build one directly instead of going through this factory.
#'
#' @param name Character. Tumour name (e.g., "Lung")
#' @param cwt_public_data_exists_for_benchmarking Logical. Whether a public
#'   CWT benchmarking bucket exists for this tumour (gates day_28/day_31/
#'   day_62 in process_benchmarking())
#' @param staging_completeness NULL, a string, or a character vector (split)
#' @param staging_demographics NULL, a string, or a character vector (split)
#' @param lwbc NULL, a string, or a character vector (split)
#' @param treatment_variation NULL or a string (the real `Tumour Site` filter
#'   value)
#' @param ... Additional fields to override (e.g. cwt_fds_detailed,
#'   cwt_provider_exclusions) — merged over the defaults below.
#' @return List matching the tumour config structure
create_mock_config <- function(
  name = "Lung",
  cwt_public_data_exists_for_benchmarking = TRUE,
  staging_completeness = name,
  staging_demographics = name,
  lwbc = name,
  treatment_variation = NULL,
  ...
) {
  cfg <- list(
    name = name,
    # Deliberately plain (not realistic "Suspected ... cancer" FDS wording):
    # defaulting every field to `name` keeps a plain create_mock_config(x)
    # matching a plain create_mock_global_data(x) (whose fixtures also default
    # to the literal cancer_type), so the common case needs no field-by-field
    # wiring. Tests that specifically exercise a field genuinely differing
    # from another (e.g. FDS wording) override it explicitly — see
    # test-tumour_processing.R's "differs from fds" tests.
    cwt_fds_public = if (cwt_public_data_exists_for_benchmarking) name else NULL,
    cwt_fds_detailed = name,
    cwt_31day_public = if (cwt_public_data_exists_for_benchmarking) name else NULL,
    cwt_31day_detailed = name,
    cwt_62day_public = if (cwt_public_data_exists_for_benchmarking) name else NULL,
    cwt_62day_detailed = name,
    # Null here matches cwt_62day_detailed being set (detailed-sourced, the
    # default) — only needed when a test overrides cwt_62day_detailed = NULL
    # to exercise the public-sourced 62-day path, in which case pass this too.
    cwt_62day_public_comm = NULL,
    cwt_public_data_exists_for_benchmarking = cwt_public_data_exists_for_benchmarking,
    display_benchmarking_fds = TRUE,
    display_benchmarking_31day = TRUE,
    display_benchmarking_62day = TRUE,
    staging_completeness = staging_completeness,
    staging_demographics = staging_demographics,
    lwbc = lwbc,
    treatment_variation = treatment_variation,
    treatment_variation_target = if (is.null(treatment_variation)) NULL else 0.5,
    treatment_variation_target_name = if (is.null(treatment_variation)) NULL else "TEST_TV_TARGET",
    cwt_provider_exclusions = NULL,
    cwt_provider_ordering = NULL
  )

  # `cfg[[field]] <- NULL` would REMOVE the element rather than setting it to
  # NULL — `[<-` with a length-1 list is the idiomatic way to overwrite with
  # NULL while keeping the key present, which callers rely on (e.g. a test
  # overriding cwt_31day_detailed = NULL to exercise the public-sourced path).
  overrides <- list(...)
  for (field in names(overrides)) cfg[field] <- overrides[field]

  cfg
}


# -----------------------------------------------------------------------------
# Global Data Creator
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Benchmarking Fixture Creators
# -----------------------------------------------------------------------------

#' Create latest 2 months benchmarking fixture
#'
#' Creates a dataframe matching the structure needed for benchmarking functions
#'
#' @param cancer_type Character. Cancer type
#' @param trusts Character vector. Trust codes
#' @return Dataframe with benchmarking structure
create_benchmarking_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS
) {
  periods <- as.Date(c("2024-05-01", "2024-06-01"))
  standards <- c("FDS", "31D", "62D")

  df <- expand.grid(
    PERIOD = periods,
    ORG.CODE = trusts,
    STANDARD = standards,
    CANCER.TYPE = cancer_type,
    TREATMENT.MODALITY = "ALL MODALITIES",
    stringsAsFactors = FALSE
  )

  # standard_compliance_calc() filters 31D to STAGE.ROUTE == "ALL STAGES" and
  # 62D to STAGE.ROUTE == "ALL ROUTES" (FDS doesn't filter on route at all)
  df$STAGE.ROUTE <- ifelse(df$STANDARD == "31D", "ALL STAGES", "ALL ROUTES")

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- sample(80:120, nrow(df), replace = TRUE)
  df$WITHIN.STANDARD <- round(df$TOTAL.TREATED * runif(nrow(df), 0.75, 0.95))

  df
}


#' Create benchmarking fixture with per-standard cancer types from config
#'
#' Each benchmarking standard may use a different CANCER.TYPE value
#' (e.g., 28-day uses "Suspected lung cancer" but 31/62-day use "Lung").
#' This function creates rows with the correct cancer type per standard.
#'
#' @param config Tumour config (resolved via get_tumour_config())
#' @param trusts Character vector. Trust codes
#' @return Dataframe with benchmarking structure
create_benchmarking_fixture_for_config <- function(
  config,
  trusts = DEFAULT_TRUSTS
) {
  periods <- as.Date(c("2024-05-01", "2024-06-01"))

  # Map each standard to its config field
  standard_fields <- list(
    "FDS" = "cwt_fds_public",
    "31D" = "cwt_31day_public",
    "62D" = "cwt_62day_public"
  )

  # standard_compliance_calc() filters 31D to STAGE.ROUTE == "ALL STAGES" and
  # 62D to STAGE.ROUTE == "ALL ROUTES" (FDS doesn't filter on route at all)
  standard_routes <- list("FDS" = "ALL ROUTES", "31D" = "ALL STAGES", "62D" = "ALL ROUTES")

  # Build rows per standard with the correct CANCER.TYPE
  dfs <- lapply(names(standard_fields), function(std) {
    cancer_types <- get_name_in_dataset(config, standard_fields[[std]])
    expand.grid(
      PERIOD = periods,
      ORG.CODE = trusts,
      STANDARD = std,
      CANCER.TYPE = cancer_types,
      STAGE.ROUTE = standard_routes[[std]],
      TREATMENT.MODALITY = "ALL MODALITIES",
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, dfs)
  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$TOTAL.TREATED <- sample(80:120, nrow(df), replace = TRUE)
  df$WITHIN.STANDARD <- round(df$TOTAL.TREATED * runif(nrow(df), 0.75, 0.95))

  df
}


#' Create latest detailed CWT fixture for FDS end reason
#'
#' @param cancer_type Character. Cancer type
#' @param trusts Character vector. Trust codes
#' @return Dataframe with latest CWT detailed structure
create_latest_cwt_detailed_fixture <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS
) {
  df <- expand.grid(
    Cancer.Report.Category = cancer_type,
    Provider.Code.FDS = trusts,
    Month = "JUN",
    Pathway.End.Reason = c("Diagnosis of cancer", "Ruling out of cancer"),
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$Provider.Code.FDS]
  df$NoPatients <- sample(20:50, nrow(df), replace = TRUE)
  df$within_standard <- round(df$NoPatients * runif(nrow(df), 0.75, 0.95))

  df
}


# -----------------------------------------------------------------------------
# LWBC Fixture Creators
# -----------------------------------------------------------------------------

#' Create LWBC plot fixture
#'
#' @param tumour_group Character. Tumour Group value
#' @return Dataframe with LWBC plot structure
create_lwbc_plot_fixture <- function(tumour_group = "Lung") {
  data.frame(
    `Tumour Group` = rep(tumour_group, 4),
    Quarter = c("Q1 23/24", "Q2 23/24", "Q3 23/24", "Q4 23/24"),
    Value = runif(4, 0.5, 0.8),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' Create LWBC trust plot fixture
#'
#' @param tumour_group Character. Tumour Group value
#' @param trusts Character vector. Trust codes
#' @return Dataframe with LWBC trust plot structure
create_lwbc_trust_plot_fixture <- function(
  tumour_group = "Lung",
  trusts = DEFAULT_TRUSTS
) {
  df <- expand.grid(
    `Tumour Group` = tumour_group,
    ORG.CODE = trusts,
    Quarter = c("Q1 23/24", "Q2 23/24"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$Value <- runif(nrow(df), 0.5, 0.8)

  names(df)[1] <- "Tumour Group"
  df
}


#' Create LWBC table fixture
#'
#' @param tumour_group Character. Tumour Group value
#' @param trusts Character vector. Trust codes
#' @return Dataframe with LWBC table structure
create_lwbc_table_fixture <- function(
  tumour_group = "Lung",
  trusts = DEFAULT_TRUSTS
) {
  df <- data.frame(
    `Tumour Group` = rep(tumour_group, length(trusts)),
    ORG.CODE = trusts,
    `Latest Value` = runif(length(trusts), 0.5, 0.8),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  df$ORG.NAME <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df
}


# -----------------------------------------------------------------------------
# LWBC PA Fixture Creators
# -----------------------------------------------------------------------------

#' Create LWBC PA plot fixture
#'
#' @param tumour_group Character. Tumour group name
#' @return Dataframe with LWBC PA plot structure
create_lwbc_pa_plot_fixture <- function(tumour_group = "Lung") {
  data.frame(
    `Tumour Group` = rep(tumour_group, 4),
    Date = seq.Date(as.Date("2024-01-01"), by = "quarter", length.out = 4),
    `Cancer Alliance` = "WMCA",
    `Total Assessed` = as.integer(runif(4, 50, 200)),
    achieved = as.integer(runif(4, 20, 100)),
    `% PA's Achieved` = runif(4, 0.4, 0.9),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' Create LWBC trust PA plot fixture
#'
#' @param tumour_group Character. Tumour group name
#' @param trusts Character vector. Trust codes
#' @return Dataframe with LWBC trust PA plot structure
create_lwbc_trust_pa_plot_fixture <- function(
  tumour_group = "Lung",
  trusts = DEFAULT_TRUSTS
) {
  df <- expand.grid(
    `Tumour Group` = tumour_group,
    ORG.CODE = trusts,
    Date = seq.Date(as.Date("2024-01-01"), by = "quarter", length.out = 2),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  df$`Cancer Alliance` <- "WMCA"
  df$`Trust Name` <- DEFAULT_TRUST_NAMES[df$ORG.CODE]
  df$`Total Assessed` <- as.integer(runif(nrow(df), 20, 100))
  df$achieved <- as.integer(runif(nrow(df), 5, 50))
  df$`% PA's Achieved` <- runif(nrow(df), 0.4, 0.9)
  names(df)[1] <- "Tumour Group"
  df
}


# -----------------------------------------------------------------------------
# Treatment Variation Fixture Creators
# -----------------------------------------------------------------------------

#' Create long-format TV fixture
#'
#' Mirrors the new `tv_long` dataframe produced by `read_in_tv.R`:
#' one row per (Tumour Site, Provider name, Time period).
#'
#' @param tumour_sites Character vector of canonical tumour names.
#' @param trusts Character vector of trust display names. Defaults to
#'   the 12 alliance trusts plus England and the alliance row.
#' @param periods Character vector of time period labels.
#' @return Dataframe matching the structure of `tv_long`.
create_tv_long_fixture <- function(
  tumour_sites = c("Lung", "Lower Gastrointestinal", "Breast", "Gynaecological"),
  trusts = unname(DEFAULT_TRUST_NAMES),
  periods = c("Q1 23/24", "Q2 23/24", "Q3 23/24", "Q4 23/24")
) {
  providers <- c(trusts, "England", "West Midlands Cancer Alliance")
  trust_codes <- c(DEFAULT_TRUSTS, NA_character_, NA_character_)

  grid <- expand.grid(
    `Tumour Site` = tumour_sites,
    `Provider name` = providers,
    `Time period` = periods,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  grid$Code <- trust_codes[match(grid$`Provider name`, providers)]
  grid$`Cancer alliance` <- "West Midlands Cancer Alliance"
  grid$Metric <- "test indicator"
  grid$`Case volume` <- 10
  grid$`Percent (%)` <- NA_real_
  grid$`Moving average (%)` <- runif(nrow(grid), 60, 90)
  grid$`Upper 95% control limit (%)` <- NA_real_
  grid$`Lower 95% control limit (%)` <- NA_real_
  grid$`Sort Key` <- match(grid$`Time period`, periods)

  grid[, c(
    "Provider name", "Code", "Cancer alliance", "Time period", "Metric",
    "Case volume", "Percent (%)", "Moving average (%)",
    "Upper 95% control limit (%)", "Lower 95% control limit (%)",
    "Sort Key", "Tumour Site"
  )]
}


#' Create indicator-strings TV fixture
#'
#' Mirrors the `tv_indicators` dataframe (one row per tumour) used by
#' `process_treatment_variation()`.
create_tv_indicators_fixture <- function() {
  data.frame(
    `Tumour Site` = c("Lung", "Lower Gastrointestinal", "Breast", "Gynaecological"),
    `Recommendation / Indicator` = c(
      "Lung indicator text",
      "LGI indicator text",
      "Breast indicator text",
      "Gynae indicator text"
    ),
    `Time Period` = rep("Q4 23/24", 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# Full Global Data Creator (for integration tests)
# -----------------------------------------------------------------------------

#' Create a complete mock global_data list for integration testing
#'
#' Creates a full global_data structure with all fixtures needed for
#' read_tumour_data() integration tests. When tumour_key is provided,
#' each dataset uses the correct raw value from name_in_dataset.
#'
#' @param cancer_type Character. Cancer type to use in fixtures (used as
#'   default when tumour_key is NULL or for datasets without a name_in_dataset)
#' @param trusts Character vector. Trust codes
#' @param periods Date vector. Periods
#' @param tumour_key Optional tumour config key. When provided, uses
#'   get_name_in_dataset() to populate each dataset with correct raw values.
#' @return List matching complete global_data structure
create_mock_global_data <- function(
  cancer_type = "Lung",
  trusts = DEFAULT_TRUSTS,
  periods = DEFAULT_PERIODS,
  tumour_key = NULL
) {
  # When a tumour_key is provided, look up the correct raw value per dataset.
  # A null config field (this tumour has no data for it — see the null /
  # scalar / array convention in config/tumours.yml's header) falls back to
  # cancer_type here: the fixture still needs SOME value to build a
  # structurally valid dataframe, even though no test should ever rely on
  # that dataframe's content for a field the tumour doesn't have.
  if (!is.null(tumour_key)) {
    cfg <- get_tumour_config(tumour_key)
    nid <- function(field) {
      val <- get_name_in_dataset(cfg, field)
      if (is.null(val)) cancer_type else val
    }
  } else {
    cfg <- NULL
    nid <- function(field) cancer_type
  }

  # Detailed FDS fixtures must resolve their cancer type the same way
  # process_fds_raw() does — cwt_fds_detailed, with no fallback under the new
  # schema — or fixtures built for a tumour whose detailed value differs from
  # cwt_fds_public would carry the wrong name while the code under test
  # filters on the detailed one, and every FDS test for that tumour would fail
  # on an empty filter rather than on a real defect.
  nid_fds_detailed <- function() {
    if (is.null(cfg)) {
      return(cancer_type)
    }
    get_name_in_dataset(cfg, "cwt_fds_detailed")
  }

  # Benchmarking: use per-standard cancer types when config available
  if (!is.null(cfg)) {
    benchmarking_df <- create_benchmarking_fixture_for_config(cfg, trusts)
  } else {
    benchmarking_df <- create_benchmarking_fixture(cancer_type, trusts)
  }

  list(
    # CWT Provider/commissioner data (public, CANCER.TYPE / Cancer Type). Read
    # by process_fds_raw()/process_31_day_raw()/process_62_day_raw() only for
    # a standard whose cwt_*_detailed is null (public-sourced); built from the
    # matching cwt_*_public/cwt_62day_public_comm config value, with the same
    # cancer_type fallback as nid() for a tumour that doesn't set it.
    fds_df = create_fds_provider_fixture(nid("cwt_fds_public"), trusts, periods),
    cwt_fds_comm_df = create_fds_comm_fixture(nid("cwt_fds_public"), trusts, periods),
    day_31_df = create_31_day_fixture(nid("cwt_31day_public"), trusts, periods),
    day_62_df = create_62_day_fixture(nid("cwt_62day_public"), trusts, periods),
    cwt_62d_comm_df = create_62_day_comm_fixture(nid("cwt_62day_public_comm"), trusts, periods),

    # CWT Detailed data
    cwt_d_fds_df = create_fds_detailed_fixture(nid_fds_detailed(), trusts, periods),
    cwt_d_fds_comm_df = create_fds_detailed_fixture(nid_fds_detailed(), trusts, periods),
    cwt_d_31_day_df = create_31_day_detailed_fixture(nid("cwt_31day_detailed"), trusts, periods),
    cwt_d_62_day_df = create_62_day_detailed_fixture(nid("cwt_62day_detailed"), trusts, periods),
    # Commissioner detailed data comes from the same sheets as the provider
    # detailed data, so it shares the same CancerReportCategory values.
    cwt_d_31_day_comm_df = create_31_day_detailed_comm_fixture(nid("cwt_31day_detailed"), DEFAULT_ICBS, periods),
    cwt_d_62_day_comm_df = create_62_day_detailed_comm_fixture(nid("cwt_62day_detailed"), DEFAULT_ICBS, periods),

    # Staging data
    stage_completeness_df = create_staging_completeness_fixture(nid("staging_completeness"), trusts),
    stage_df = create_staging_demographics_fixture(nid("staging_demographics")),

    # Benchmarking data — each standard may use a different raw cancer type
    latest_public_2_months_df = benchmarking_df,
    latest_cwt_d_df = create_latest_cwt_detailed_fixture(nid_fds_detailed(), trusts),

    # LWBC data
    tumour_pack_lwbc_plot_df = create_lwbc_plot_fixture(nid("lwbc")),
    tumour_pack_lwbc_pa_plot_df = create_lwbc_pa_plot_fixture(nid("lwbc")),
    tumour_pack_trust_lwbc_plot_df = create_lwbc_trust_plot_fixture(nid("lwbc"), trusts),
    tumour_pack_trust_lwbc_pa_plot_df = create_lwbc_trust_pa_plot_fixture(nid("lwbc"), trusts),
    tumour_pack_alliance_trust_lwbc_table_df = create_lwbc_table_fixture(nid("lwbc"), trusts),

    # Treatment variation data
    tv_long = create_tv_long_fixture(),
    tv_indicators = create_tv_indicators_fixture()
  )
}
