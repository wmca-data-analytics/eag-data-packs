### Treatment Variation Data ###################################################
################################################################################
################################################################################
#
# Source workbook is long format. Sheet `All Data` (hidden) carries every
# tumour/provider/period as rows. Sheet `WMCA` rows 16-22 hold the
# human-friendly indicator strings and the chosen display period.
#
# This script reads both, applies the canonical alliance name map, and
# leaves two dataframes (`tv_long`, `tv_indicators`) in the calling
# environment for downstream processing.

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

tv_config <- get_tv_config()
file_path <- tv_config$file_path

# ============================================================================
# Long-format performance data (sheet "All Data", hidden, header on row 3)
# ============================================================================

tv_long <- read_excel(
  file_path,
  sheet = tv_config$all_data_sheet,
  skip  = 2
)

# Strip GCS prefix so Ovarian regional-centre rows share the standard NHS code
tv_long <- tv_long |>
  mutate(Code = sub("^GCS", "", Code))

# Recode Provider name from Code (canonical, via org_lookup). Mapping by Code
# avoids the slight Provider-name variants in the Ovarian rows ("The "
# prefix, "Of"/"of" case differences).
tv_code_to_name <- get_org_recode_map()
tv_alliance_codes <- unname(get_alliance_config("tv_geography_map"))
tv_long <- tv_long |>
  mutate(
    `Provider name` = ifelse(
      !is.na(Code) & Code %in% tv_alliance_codes,
      tv_code_to_name[Code],
      `Provider name`
    )
  )

# Numeric Sort Key for chronological ordering
tv_long <- tv_long |>
  mutate(`Sort Key` = as.integer(`Sort Key`))

# Drop rows for tumour sites we don't consume
tv_long <- tv_long |>
  filter(`Tumour Site` %in% c("Lung", "Bowel", "Primary Breast", "Ovarian"))

# Normalise Tumour Site to canonical tumour config names
tv_cancer_mapping <- c(
  "Primary Breast" = "Breast",
  "Bowel"          = "Lower Gastrointestinal",
  "Ovarian"        = "Gynaecological"
)
tv_long <- tv_long |>
  mutate(`Tumour Site` = ifelse(
    `Tumour Site` %in% names(tv_cancer_mapping),
    tv_cancer_mapping[`Tumour Site`],
    `Tumour Site`
  ))

# ============================================================================
# Indicator strings + chosen display period (sheet "WMCA", rows 16-22 cols A-C)
# ============================================================================

tv_indicators <- read_excel(
  file_path,
  sheet     = tv_config$indicator_sheet,
  range     = tv_config$indicator_range,
  col_names = c("Tumour Site", "Recommendation / Indicator", ".drop", "Time Period")
)

tv_indicators <- tv_indicators |>
  select(-`.drop`) |>
  filter(`Tumour Site` %in% c("Lung", "Bowel", "Primary Breast", "Ovarian")) |>
  mutate(`Tumour Site` = ifelse(
    `Tumour Site` %in% names(tv_cancer_mapping),
    tv_cancer_mapping[`Tumour Site`],
    `Tumour Site`
  ))

rm(tv_code_to_name, tv_alliance_codes, tv_cancer_mapping)
