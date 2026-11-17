# ******************************************************************************
# ************************  ALLIANCE CONFIG LOADER  ****************************
# ******************************************************************************
#
# Loads alliance configuration from config/alliances.yml. Only the alliance
# whose `code:` field matches the global `alliance` variable is loaded.
# Set the global `alliance` (e.g. alliance <- "WMCA") BEFORE sourcing this file.
#
# After sourcing, .alliance_configs is a single-entry named list keyed by
# alliance code, so existing accessors (get_alliance_config(), ...) work
# unchanged.
#
# After loading, certain fields are normalised:
#   - trust_codes                 list -> character vector
#   - org_lookup                  list of cols -> data.frame
#   - stage_geography_map,
#     lwbc_geography_map,
#     tv_geography_map,
#     icb_recode_map,
#     trust_to_icb                named list -> named character vector
#
# Other fields (tv_config, metadata) are left as-is. In particular,
# tv_config$sheet_names stays a named list because consumers access it with
# the $ operator (e.g. tv_config$sheet_names$lung).
#
# Helper accessors (get_alliance_config(), get_org_recode_map(), ...) live in
# R/data_pack_utils.R.

# ---- Locate config directory ------------------------------------------------
# Defined here too (rather than relying on load_globals.R) so this file can be
# sourced independently. Tolerates project root or one/two levels deeper.
if (!exists(".find_config_dir", inherits = TRUE)) {
  .find_config_dir <- function() {
    candidates <- c("config",
                    file.path("..", "config"),
                    file.path("..", "..", "config"))
    for (p in candidates) if (dir.exists(p)) return(p)
    stop("Could not locate config/ directory from working dir: ", getwd())
  }
}

if (!exists(".yaml_handlers", inherits = TRUE)) {
  .yaml_handlers <- list(int = function(x) as.numeric(x))
}

.unlist_named_map <- function(x) {
  if (is.null(x)) return(NULL)
  unlist(x)
}

.post_process_alliance <- function(cfg) {
  cfg$trust_codes <- unlist(cfg$trust_codes, use.names = FALSE)

  for (k in c("stage_geography_map", "lwbc_geography_map", "tv_geography_map",
              "icb_recode_map", "trust_to_icb")) {
    cfg[[k]] <- .unlist_named_map(cfg[[k]])
  }

  cfg$org_lookup <- as.data.frame(
    lapply(cfg$org_lookup, unlist, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  cfg
}

#' Load a single alliance configuration by code
#'
#' Reads config/alliances.yml and returns the post-processed configuration for
#' the requested alliance. Used internally to populate .alliance_configs, and
#' available to tests that need to inspect multiple alliances independently.
#'
#' @param code Alliance code (e.g. "WMCA", "EMCA"). Case-insensitive.
#' @return Post-processed configuration list for the requested alliance.
load_alliance_config <- function(code) {
  config_dir <- .find_config_dir()
  raw <- yaml::read_yaml(file.path(config_dir, "alliances.yml"),
                         handlers = .yaml_handlers)
  code_up <- toupper(code)
  if (!code_up %in% names(raw)) {
    stop(paste0("Alliance '", code, "' not found in config/alliances.yml. ",
                "Available: ", paste(names(raw), collapse = ", ")))
  }
  .post_process_alliance(raw[[code_up]])
}

# ---- Populate .alliance_configs with the active alliance --------------------
# Requires the `alliance` variable to be set in the calling scope (the global
# environment in production via main.R, or the testthat setup environment for
# tests). Errors loudly if missing rather than silently loading nothing.
if (!exists("alliance", inherits = TRUE)) {
  stop(
    "'alliance' variable not set. ",
    "Set it (e.g. alliance <- 'WMCA') before sourcing R/load_alliance_config.R"
  )
}

local({
  code <- toupper(get("alliance", inherits = TRUE))
  .alliance_configs <<- setNames(list(load_alliance_config(code)), code)
})
