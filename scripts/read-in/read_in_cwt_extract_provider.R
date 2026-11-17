### Detailed Cancer Waiting Times Data #########################################
################################################################################
################################################################################

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

# Data path
cwt_d_path <- "data/cwt-detailed/provider"

# Validate data path exists
if (!dir.exists(cwt_d_path)) {
  stop(paste("CWT detailed provider data directory not found:", cwt_d_path), call. = FALSE)
}

# Get alliance and recode map from centralised config
alliance_name <- get_alliance_config("alliance")
org_recode_map <- get_org_recode_map()

# Get all files for the alliance (used by all sections)
cwt_d_files <- get_alliance_files(cwt_d_path, alliance_name)

# Validate files were found
if (length(cwt_d_files) == 0) {
  stop(
    paste0(
      "No CWT detailed provider files found for alliance '", alliance_name, "'\n",
      "Path searched: ", cwt_d_path, "\n",
      "Expected pattern: ", alliance_name, "_provider_monthly_*.xlsx"
    ),
    call. = FALSE
  )
}

message(paste("Found", length(cwt_d_files), "CWT detailed provider files for alliance:", alliance_name))

# ============================================================================
# 28-DAY FDS Data
# ============================================================================

# Read all 28-DAY FDS data from all files
cwt_d_fds_master_df <- read_sheets_from_files(cwt_d_files, "28-DAY FDS")

# Process date format
cwt_d_fds_df <- cwt_d_fds_master_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Add organisation names using provider codes
cwt_d_fds_df <- cwt_d_fds_df %>%
  mutate(ORG.NAME = recode(Provider.Code.FDS, !!!org_recode_map))

# Clean up
rm(cwt_d_fds_master_df)

# Latest reporting month for the pack, derived from the detailed extract.
# The public CWT files are no longer published monthly, so the detailed data is # the authoritative source for "what month is this pack about?".
if (all(is.na(cwt_d_fds_df$Date))) {
  stop(
    paste0(
      "No parseable dates in detailed CWT provider FDS data.\n",
      "Check the Month/Year columns in: ", paste(basename(cwt_d_files), collapse = ", ")
    ),
    call. = FALSE
  )
}

if (any(is.na(cwt_d_fds_df$Date))) {
  warning(
    paste0(
      sum(is.na(cwt_d_fds_df$Date)), " row(s) of detailed CWT provider FDS data have an ",
      "unparseable Month/Year and are excluded from the reporting month calculation."
    ),
    call. = FALSE
  )
}

latest_month <- max(cwt_d_fds_df$Date, na.rm = TRUE)

latest_cwt_d_df <- cwt_d_fds_df |>
  filter(Date == max(Date))

latest_cwt_d_df <- latest_cwt_d_df |>
  select(
    Provider.Code.FDS,
    Month,
    Year,
    Pathway.End.Reason,
    Cancer.Report.Category,
    NoPatients,
    DaysWithin7,
    Days8To14,
    Days15To21,
    Days22To28,
    Trust_Name
  )

latest_cwt_d_df <- latest_cwt_d_df |>
  mutate(
    within_standard = rowSums(across(c(DaysWithin7, Days8To14, Days15To21, Days22To28)), na.rm = TRUE)
  )

# ============================================================================
# 31-DAY Data
# ============================================================================

# Read all 31-DAY data from all files
cwt_d_31_day_master_df <- read_sheets_from_files(cwt_d_files, "31-DAY")

# Add organisation names using provider codes
cwt_d_31_day_df <- cwt_d_31_day_master_df %>%
  mutate(ORG.NAME = recode(ProviderCode, !!!org_recode_map))

# Process date format
cwt_d_31_day_df <- cwt_d_31_day_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Clean up
rm(cwt_d_31_day_master_df)

# ============================================================================
# 62-DAY Data (CLASSIC, SCREENING, UPGRADE combined)
# ============================================================================

# Read all 62-DAY data from all files (all three sheet types)
cwt_d_62_day_master_df <- read_sheets_from_files(
  cwt_d_files,
  c("62-DAY CLASSIC", "62-DAY SCREENING", "62-DAY UPGRADE")
)

# Add organisation names using provider codes
cwt_d_62_day_df <- cwt_d_62_day_master_df %>%
  mutate(ORG.NAME = recode(AccountableProvider, !!!org_recode_map))

# Process date format
cwt_d_62_day_df <- cwt_d_62_day_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Clean up
rm(cwt_d_62_day_master_df)
