### Detailed Cancer Waiting Times Data #########################################
### Commissioner ###############################################################
################################################################################

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

# Data path
cwt_d_comm_path <- "data/cwt-detailed/commissioner"

# Validate data path exists
if (!dir.exists(cwt_d_comm_path)) {
  stop(paste("CWT detailed commissioner data directory not found:", cwt_d_comm_path), call. = FALSE)
}

# Get alliance and ICB recode map from centralised config
# Note: Uses legacy CCG codes from older datasets, mapped to ICB regions
alliance_name <- get_alliance_config("alliance")
org_recode_map_comm <- get_alliance_config("icb_recode_map")

# Get all commissioner files for the alliance (used by all sections)
cwt_d_comm_files <- get_alliance_files(cwt_d_comm_path, alliance_name, "commissioner")

# Validate files were found
if (length(cwt_d_comm_files) == 0) {
  stop(
    paste0(
      "No CWT detailed commissioner files found for alliance '", alliance_name, "'\n",
      "Path searched: ", cwt_d_comm_path, "\n",
      "Expected pattern: ", alliance_name, "_commissioner_monthly_*.xlsx"
    ),
    call. = FALSE
  )
}

message(paste("Found", length(cwt_d_comm_files), "CWT detailed commissioner files for alliance:", alliance_name))

# ============================================================================
# 28-DAY FDS Data (Commissioner)
# ============================================================================

# Read all 28-DAY FDS data from all files
cwt_d_fds_master_comm_df <- read_sheets_from_files(cwt_d_comm_files, "28-DAY FDS")

# Process date format
cwt_d_fds_comm_df <- cwt_d_fds_master_comm_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Add organisation names using CCG codes
cwt_d_fds_comm_df <- cwt_d_fds_comm_df %>%
  mutate(ORG.NAME = recode(CCG, !!!org_recode_map_comm))

# Clean up
rm(cwt_d_fds_master_comm_df)

# ============================================================================
# 31-DAY Data (Commissioner)
# ============================================================================

# Read all 31-DAY data from all files
cwt_d_31_day_master_comm_df <- read_sheets_from_files(cwt_d_comm_files, "31-DAY")

# Add organisation names using CCG codes
cwt_d_31_day_comm_df <- cwt_d_31_day_master_comm_df %>%
  mutate(ORG.NAME = recode(CCG, !!!org_recode_map_comm))

# Process date format
cwt_d_31_day_comm_df <- cwt_d_31_day_comm_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Clean up
rm(cwt_d_31_day_master_comm_df)

# ============================================================================
# 62-DAY Data (Commissioner) - CLASSIC, SCREENING, UPGRADE combined
# ============================================================================

# Read all 62-DAY data from all files (all three sheet types)
cwt_d_62_day_master_comm_df <- read_sheets_from_files(
  cwt_d_comm_files,
  c("62-DAY CLASSIC", "62-DAY SCREENING", "62-DAY UPGRADE")
)

# Add organisation names using CCG codes
cwt_d_62_day_comm_df <- cwt_d_62_day_master_comm_df %>%
  mutate(ORG.NAME = recode(CCG, !!!org_recode_map_comm))

# Process date format
cwt_d_62_day_comm_df <- cwt_d_62_day_comm_df %>%
  mutate(Date = paste0(Month, " ", Year)) %>%
  mutate(Date = parse_financial_year_date(Date))

# Clean up
rm(cwt_d_62_day_master_comm_df)
