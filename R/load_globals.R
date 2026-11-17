# ******************************************************************************
# *****************************  GLOBALS LOADER  *******************************
# ******************************************************************************
#
# Loads scalar globals (CWT_*, STAGING_*, TV_*, SLIDE_FOOTER_*) from
# config/globals.yml. Each top-level YAML key is assigned as a global variable
# in .GlobalEnv with the same name.
#
# Edit config/globals.yml to change values - no code changes needed.

# ---- Locate config directory ------------------------------------------------
# Tolerate being sourced from project root or one/two levels deeper (tests).
.find_config_dir <- function() {
  candidates <- c("config",
                  file.path("..", "config"),
                  file.path("..", "..", "config"))
  for (p in candidates) if (dir.exists(p)) return(p)
  stop("Could not locate config/ directory from working dir: ", getwd())
}

# yaml::read_yaml parses bare integers as R `integer` (e.g. 80L); the original
# hand-written R code used `double` (e.g. 80) for these constants. To preserve
# the exact original types, convert all parsed integers to doubles on load.
.yaml_handlers <- list(int = function(x) as.numeric(x))

local({
  config_dir <- .find_config_dir()
  globals <- yaml::read_yaml(file.path(config_dir, "globals.yml"),
                             handlers = .yaml_handlers)
  for (name in names(globals)) {
    assign(name, globals[[name]], envir = .GlobalEnv)
  }
})
