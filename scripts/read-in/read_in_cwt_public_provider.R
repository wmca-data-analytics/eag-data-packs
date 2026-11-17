### Cancer Waiting Times Data (Public, Provider Level) ##########################
################################################################################
################################################################################
#
# Source: NHS England "Monthly Combined Workbook" (Provider Level Performance
# sheet), from the Cancer Waiting Times monthly statistics release. This is a
# single workbook per month with no time series within it — month-on-month
# rank comparisons rely on keeping more than one month's workbook side-by-side
# in cwt_path (the read-in below combines every .xlsx file found there).
#
# Each workbook has a 13-row metadata block, a header on row 15, and no
# PERIOD column in the data rows — the reporting month is read from the
# "Period:" metadata cell (row 5, col C) instead.

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

# Data path
cwt_path <- "data/cwt/provider"

# Validate data path exists
if (!dir.exists(cwt_path)) {
  stop(paste("CWT provider data directory not found:", cwt_path), call. = FALSE)
}

# Get organisation codes and recode map from centralised config
org_codes <- get_alliance_config("trust_codes")
org_recode_map <- get_org_recode_map()

# Validate configuration
if (length(org_codes) == 0) {
  stop("No organisation codes returned by get_alliance_config(\"trust_codes\")", call. = FALSE)
}

message(paste("Loading CWT provider data for", length(org_codes), "organisations"))

# ============================================================================
# Read All CWT Data
# ============================================================================

cwt_files <- list.files(path = cwt_path, pattern = "\\.xlsx$", full.names = TRUE)

if (length(cwt_files) == 0) {
  stop(paste("No CWT provider Excel files (.xlsx) found in:", cwt_path), call. = FALSE)
}

# Provider-level performance rows (skip the 14-row metadata block above the header)
all_combined_data <- read_excel_files(
  cwt_path,
  sheet = "Provider Level Performance",
  skip  = 14
)

# Look up each file's reporting month from its "Period:" metadata cell
# (row 5, col C — e.g. "May 2026") and stamp it onto that file's rows.
file_periods <- vapply(cwt_files, function(file) {
  period_cell <- suppressMessages(
    readxl::read_excel(
      file,
      sheet      = "Provider Level Performance",
      range      = "C5",
      col_names  = FALSE
    )
  )
  as.character(period_cell[[1]][1])
}, character(1))
names(file_periods) <- basename(cwt_files)

all_combined_data <- all_combined_data %>%
  mutate(PERIOD = lubridate::my(file_periods[SOURCE.FILE])) %>%
  select(-SOURCE.FILE)

# Drop the trailing blank row(s) some workbooks carry after the last data row,
# and the "Total" aggregate row (England total) — a fake "organisation" mixed
# in among the real trusts that would distort national ranking/summation if
# left in.
all_combined_data <- all_combined_data %>%
  filter(!is.na(`Org Code`), `Org Code` != "Total")

# Standardise column names to the internal schema used throughout the pipeline
all_combined_data <- all_combined_data %>%
  rename(
    ORG.CODE           = `Org Code`,
    STANDARD           = Standard,
    CANCER.TYPE        = `Cancer Type`,
    STAGE.ROUTE        = `Referral Route/Stage`,
    TREATMENT.MODALITY = `Treatment Modality`,
    WITHIN.STANDARD    = Within,
    TOTAL.TREATED      = Total
  )

# Validate data was loaded
message(paste("Loaded", nrow(all_combined_data), "rows of CWT provider data"))

# Arrange by PERIOD descending
all_combined_data <- all_combined_data |> arrange(desc(PERIOD))

# ============================================================================
# Alliance-Filtered Data
# ============================================================================

# Filter for organisation codes and add organisation names
cwt_df <- all_combined_data %>%
  filter(ORG.CODE %in% org_codes) %>%
  mutate(ORG.NAME = recode(ORG.CODE, !!!org_recode_map)) %>%
  arrange(PERIOD)

# Get FDS data
fds_df <- cwt_df %>%
  filter(STANDARD == "FDS")

# Get 31-day data
day_31_df <- cwt_df %>%
  filter(STANDARD == "31D")

# Get 62-day data
day_62_df <- cwt_df %>%
  filter(STANDARD == "62D")

# ============================================================================
# Latest 2 Months Data (All Providers)
# ============================================================================

# Extract latest 2 months data
latest_public_month <- max(all_combined_data$PERIOD, na.rm = TRUE)
latest_public_2_months_df <- all_combined_data |>
  filter(PERIOD >= latest_public_month %m-% months(1))
