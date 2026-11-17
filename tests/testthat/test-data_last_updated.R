# Tests for scripts/data_last_updated.R
# =============================================================================
# This script has no functions - it's straight-line code that lists
# getwd()/data, grouping and printing results. We test it by sourcing it
# with the working directory pointed at a throwaway fixture tree, and
# asserting on the printed (captured) output.

run_data_last_updated <- function(data_dir_setup) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  data_dir <- file.path(dir, "data")
  dir.create(data_dir)
  data_dir_setup(data_dir)

  withr::with_dir(dir, {
    capture.output(sys.source(file.path(project_root, "scripts", "data_last_updated.R"),
                               envir = new.env(parent = parent.env(globalenv()))))
  })
}

test_that("groups root-level files under '(root of data)'", {
  out <- run_data_last_updated(function(data_dir) {
    writeLines("x", file.path(data_dir, "root.csv"))
  })

  expect_true(any(grepl("^============== \\(root of data\\) ==============$", out)))
  expect_true(any(grepl("root\\.csv", out)))
})

test_that("groups files by immediate subfolder, using the subfolder name as the header", {
  out <- run_data_last_updated(function(data_dir) {
    dir.create(file.path(data_dir, "cwt"))
    writeLines("x", file.path(data_dir, "cwt", "a.xlsx"))
  })

  expect_true(any(grepl("^============== cwt ==============$", out)))
  expect_true(any(grepl("a\\.xlsx", out)))
})

test_that("prints a MODIFIED/FILENAME header once per group, even with multiple files", {
  out <- run_data_last_updated(function(data_dir) {
    dir.create(file.path(data_dir, "cwt"))
    writeLines("x", file.path(data_dir, "cwt", "a.xlsx"))
    writeLines("y", file.path(data_dir, "cwt", "b.xlsx"))
  })

  header_lines <- grep("MODIFIED\\s+FILENAME", out)
  expect_length(header_lines, 1)
  expect_true(any(grepl("a\\.xlsx", out)))
  expect_true(any(grepl("b\\.xlsx", out)))
})

test_that("sorts output by subfolder then filename", {
  out <- run_data_last_updated(function(data_dir) {
    dir.create(file.path(data_dir, "zzz_folder"))
    dir.create(file.path(data_dir, "aaa_folder"))
    writeLines("x", file.path(data_dir, "zzz_folder", "f.csv"))
    writeLines("x", file.path(data_dir, "aaa_folder", "b.csv"))
    writeLines("x", file.path(data_dir, "aaa_folder", "a.csv"))
  })

  group_headers <- grep("^==============", out)
  expect_equal(out[group_headers], c(
    "============== aaa_folder ==============",
    "============== zzz_folder =============="
  ))

  aaa_block <- out[group_headers[1]:(group_headers[2] - 1)]
  expect_true(which(grepl("a\\.csv", aaa_block)) < which(grepl("b\\.csv", aaa_block)))
})

test_that("formats the modified date as 'DD Month YYYY'", {
  out <- run_data_last_updated(function(data_dir) {
    writeLines("x", file.path(data_dir, "root.csv"))
  })

  data_line <- out[grepl("root\\.csv", out)]
  expect_match(data_line, "^\\d{2} [A-Za-z]+ \\d{4}\\s+root\\.csv\\s*$")
})

test_that("produces no output for an empty data/ directory", {
  out <- run_data_last_updated(function(data_dir) {
    # leave data_dir empty
  })

  expect_length(out, 0)
})

test_that("recurses into nested subfolders, grouping by the immediate parent", {
  out <- run_data_last_updated(function(data_dir) {
    nested <- file.path(data_dir, "cwt", "provider")
    dir.create(nested, recursive = TRUE)
    writeLines("x", file.path(nested, "c.xlsx"))
  })

  expect_true(any(grepl("^============== cwt(/|\\\\)provider ==============$", out)))
  expect_true(any(grepl("c\\.xlsx", out)))
})
