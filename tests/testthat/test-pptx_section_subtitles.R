# Tests for R/pptx_section_subtitles.R
#
# add_section_subtitles() operates on a real .pptx file on disk (it reads and
# overwrites it via officer), so these tests build a small fixture pptx
# directly with officer against the real reference_doc, rather than rendering
# the full Tumour_Data_Pack_Template.Rmd (which needs flextable, an optional
# system dependency skipped elsewhere in this suite when unavailable).

has_officer <- requireNamespace("officer", quietly = TRUE)
reference_doc <- file.path(project_root, "assets", "pptx-templates", "WMCA_EAG_template.pptx")
has_reference_doc <- has_officer && file.exists(reference_doc)

#' Build a small fixture .pptx for testing add_section_subtitles()
#'
#' @param section_title Character. Title for a "Section Header" slide.
#' @param section_body Character or NULL. Pre-existing body text for that
#'   slide, or NULL to leave it blank (the normal case before injection).
#' @return Path to the saved fixture .pptx file.
create_subtitle_fixture <- function(section_title = "Cancer Waiting Times", section_body = NULL) {
  doc <- officer::read_pptx(reference_doc)

  doc <- officer::add_slide(doc, layout = "Section Header", master = "Office Theme")
  doc <- officer::ph_with(doc, value = section_title, location = officer::ph_location_type(type = "title"))
  if (!is.null(section_body)) {
    doc <- officer::ph_with(doc, value = section_body, location = officer::ph_location_type(type = "body"))
  }

  # An ordinary content slide, to confirm it's never touched
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, value = "FDS Summary", location = officer::ph_location_type(type = "title"))
  doc <- officer::ph_with(doc, value = "Real body content here", location = officer::ph_location_type(type = "body"))

  f <- tempfile(fileext = ".pptx")
  print(doc, target = f)
  f
}

#' Read back a fixture .pptx's slides as a list of type/text dataframes
read_fixture_slides <- function(path) {
  doc <- officer::read_pptx(path)
  lapply(seq_len(length(doc)), function(i) {
    doc <- officer::on_slide(doc, index = i)
    officer::slide_summary(doc)[, c("type", "text")]
  })
}

#' Find the one slide (among read_fixture_slides()'s output) with a given
#' title, trimmed of officer's surrounding whitespace
#'
#' read_pptx(reference_doc) keeps whatever slides the template file itself
#' already contains, and add_slide() appends after them — so the fixture
#' slides added by create_subtitle_fixture() are not necessarily at fixed
#' indices 1, 2, ...; this looks them up by title instead of assuming a
#' position.
find_slide_by_title <- function(slides, title) {
  for (s in slides) {
    title_row <- s[s$type %in% c("title", "ctrTitle"), , drop = FALSE]
    if (nrow(title_row) > 0 && trimws(title_row$text[1]) == title) {
      return(s)
    }
  }
  NULL
}


# =============================================================================
# get_section_subtitles()
# =============================================================================

test_that("get_section_subtitles() returns the five section titles with correct text", {
  result <- get_section_subtitles("lung")

  expect_named(
    result,
    c("Cancer Waiting Times", "Treatment Variation", "Staging Completeness",
      "Early Staging", "Living With and Beyond Cancer")
  )
  expect_equal(result[["Cancer Waiting Times"]], format_cwt_filter_subtitle("lung"))
  expect_equal(result[["Treatment Variation"]], format_split_aware_filter_subtitle("lung", "treatment_variation"))
  expect_equal(result[["Staging Completeness"]], format_split_aware_filter_subtitle("lung", "staging_completeness"))
  expect_equal(result[["Early Staging"]], format_split_aware_filter_subtitle("lung", "staging_demographics"))
  expect_equal(result[["Living With and Beyond Cancer"]], format_split_aware_filter_subtitle("lung", "lwbc"))
})

test_that("get_section_subtitles() splits staging fields where urology does", {
  result <- get_section_subtitles("urology")

  expect_equal(
    result[["Staging Completeness"]],
    format_split_aware_filter_subtitle("urology", "staging_completeness")
  )
  expect_equal(
    result[["Early Staging"]],
    format_split_aware_filter_subtitle("urology", "staging_demographics")
  )
  expect_true(any(grepl("Urological", result[["Staging Completeness"]])))
  expect_true(any(grepl("Prostate", result[["Staging Completeness"]])))
})


# =============================================================================
# get_slide_layout_names()
# =============================================================================

test_that("get_slide_layout_names() returns the correct layout name per slide, in order", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  doc <- officer::read_pptx(reference_doc)
  doc <- officer::add_slide(doc, layout = "Section Header", master = "Office Theme")
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")

  fixture <- tempfile(fileext = ".pptx")
  print(doc, target = fixture)
  on.exit(unlink(fixture))

  doc2 <- officer::read_pptx(fixture)
  layout_names <- get_slide_layout_names(doc2)

  n <- length(doc2)
  expect_equal(layout_names[n - 1], "Section Header")
  expect_equal(layout_names[n], "Title and Content")
})


# =============================================================================
# add_section_subtitles()
# =============================================================================

test_that("add_section_subtitles() writes the subtitle into a blank Section Header slide", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  fixture <- create_subtitle_fixture("Cancer Waiting Times", section_body = NULL)
  on.exit(unlink(fixture))

  add_section_subtitles(fixture, "lung")

  slides <- read_fixture_slides(fixture)

  # format_cwt_filter_subtitle() returns one element per PowerPoint paragraph
  # (real line breaks); slide_summary() reads them back concatenated into one
  # string with no separator, so the expected side is collapsed the same way.
  cwt_slide <- find_slide_by_title(slides, "Cancer Waiting Times")
  cwt_body <- cwt_slide[cwt_slide$type == "body", "text"]
  expect_equal(trimws(cwt_body), paste(format_cwt_filter_subtitle("lung"), collapse = ""))

  # The ordinary content slide is completely untouched
  content_slide <- find_slide_by_title(slides, "FDS Summary")
  content_body <- content_slide[content_slide$type == "body", "text"]
  expect_equal(trimws(content_body), "Real body content here")
})

test_that("add_section_subtitles() skips a matching title whose body already has content, with a warning", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  fixture <- create_subtitle_fixture("Cancer Waiting Times", section_body = "Already has content")
  on.exit(unlink(fixture))

  expect_warning(
    add_section_subtitles(fixture, "lung"),
    regexp = "Skipping section subtitle.*Cancer Waiting Times"
  )

  slides <- read_fixture_slides(fixture)
  cwt_slide <- find_slide_by_title(slides, "Cancer Waiting Times")
  cwt_body <- cwt_slide[cwt_slide$type == "body", "text"]
  expect_equal(trimws(cwt_body), "Already has content")
})

test_that("add_section_subtitles() leaves a slide alone when its title matches no known section", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  fixture <- create_subtitle_fixture("Not A Real Section", section_body = NULL)
  on.exit(unlink(fixture))

  expect_no_warning(add_section_subtitles(fixture, "lung"))

  slides <- read_fixture_slides(fixture)
  not_a_section_slide <- find_slide_by_title(slides, "Not A Real Section")
  expect_equal(nrow(not_a_section_slide), 1)  # title only, no body placeholder added
})

test_that("add_section_subtitles() gives every tab-containing subtitle line an explicit tab stop", {
  # format_subtitle_lines() (R/load_tumour_config.R) separates every
  # subtitle line's label from its value with a literal tab character so all
  # values align in one column — but a bare tab
  # with no tab stop falls back to PowerPoint's default 1-inch increments,
  # which does not reliably land every line at the same column (confirmed by
  # inspecting generated XML: officer's fp_par(tabs = ...) is not serialised
  # for pptx by the installed officer version). add_tab_stop_to_body() must
  # inject a real <a:tabLst> directly. Checked against the actual saved XML,
  # not slide_summary() (which reports text only, not paragraph properties).
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  fixture <- create_subtitle_fixture("Cancer Waiting Times", section_body = NULL)
  on.exit(unlink(fixture))

  add_section_subtitles(fixture, "lung")

  unzipped <- tempfile()
  utils::unzip(fixture, exdir = unzipped)
  slide_files <- list.files(file.path(unzipped, "ppt", "slides"), pattern = "\\.xml$", full.names = TRUE)
  cwt_slide_xml <- Filter(function(f) any(grepl("Cancer Waiting Times", readLines(f, warn = FALSE))), slide_files)
  expect_length(cwt_slide_xml, 1)

  xml_text <- paste(readLines(cwt_slide_xml, warn = FALSE), collapse = "")
  paragraphs <- regmatches(xml_text, gregexpr("<a:p>.*?</a:p>", xml_text))[[1]]
  tab_paragraphs <- Filter(function(p) grepl("\t", p, fixed = TRUE), paragraphs)

  # "lung" shares one Source across all three CWT standards (collapsed to one
  # line, still tab-separated so its value aligns with the Filters column)
  # and Filter always splits into three tab-containing lines (FDS, 31-day,
  # 62-day)
  expect_length(tab_paragraphs, 4)
  for (p in tab_paragraphs) {
    expect_match(p, "<a:tabLst>\\s*<a:tab pos=\"[0-9]+\" algn=\"l\"/>\\s*</a:tabLst>")
  }
})

test_that("add_tab_stop_to_body() only adds a tab stop to paragraphs that actually contain a tab", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  doc <- officer::read_pptx(reference_doc)
  doc <- officer::add_slide(doc, layout = "Section Header", master = "Office Theme")
  doc <- officer::ph_with(doc, value = c("No tab here", "Has\ta tab"),
                           location = officer::ph_location_type(type = "body"))

  add_tab_stop_to_body(doc, length(doc))

  fixture <- tempfile(fileext = ".pptx")
  print(doc, target = fixture)
  on.exit(unlink(fixture))

  unzipped <- tempfile()
  utils::unzip(fixture, exdir = unzipped)
  slide_files <- list.files(file.path(unzipped, "ppt", "slides"), pattern = "\\.xml$", full.names = TRUE)
  xml_text <- paste(readLines(slide_files[length(slide_files)], warn = FALSE), collapse = "")
  paragraphs <- regmatches(xml_text, gregexpr("<a:p>.*?</a:p>", xml_text))[[1]]

  expect_length(paragraphs, 2)
  no_tab_p <- Filter(function(p) grepl("No tab here", p, fixed = TRUE), paragraphs)[[1]]
  has_tab_p <- Filter(function(p) grepl("Has\ta tab", p, fixed = TRUE), paragraphs)[[1]]

  expect_false(grepl("tabLst", no_tab_p, fixed = TRUE))
  expect_match(has_tab_p, "<a:tabLst>\\s*<a:tab pos=\"[0-9]+\" algn=\"l\"/>\\s*</a:tabLst>")
})

test_that("add_section_subtitles() skips a Section Header slide with no title placeholder written", {
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  doc <- officer::read_pptx(reference_doc)
  # Deliberately never call ph_with() on this slide at all: slide_summary()
  # then reports zero rows for it (no title, no body - not even an empty
  # placeholder), exercising the nrow(title_row) == 0 guard.
  doc <- officer::add_slide(doc, layout = "Section Header", master = "Office Theme")

  fixture <- tempfile(fileext = ".pptx")
  print(doc, target = fixture)
  on.exit(unlink(fixture))

  expect_no_warning(expect_no_error(add_section_subtitles(fixture, "lung")))
})

test_that("add_section_subtitles() only writes the Section Header slide when a content slide reuses its exact title", {
  # Reproduces the real bug: "# Early Staging" (Section Header layout) is
  # immediately followed by "## Early Staging" (Title and Content layout, the
  # first chart) with the identical title text. Title-only matching hit both;
  # the layout check must restrict the write to the genuine divider slide.
  skip_if_not(has_reference_doc, "reference_doc pptx template not available")

  doc <- officer::read_pptx(reference_doc)

  doc <- officer::add_slide(doc, layout = "Section Header", master = "Office Theme")
  doc <- officer::ph_with(doc, value = "Early Staging", location = officer::ph_location_type(type = "title"))

  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, value = "Early Staging", location = officer::ph_location_type(type = "title"))
  # Deliberately no body text added: mirrors a chart slide whose body
  # placeholder holds only an image, which reads as empty text to officer.

  fixture <- tempfile(fileext = ".pptx")
  print(doc, target = fixture)
  on.exit(unlink(fixture))

  expect_no_warning(add_section_subtitles(fixture, "lung"))

  doc2 <- officer::read_pptx(fixture)
  layout_names <- get_slide_layout_names(doc2)
  early_staging_slides <- Filter(function(i) {
    doc2 <- officer::on_slide(doc2, index = i)
    s <- officer::slide_summary(doc2)
    title_row <- s[s$type %in% c("title", "ctrTitle"), , drop = FALSE]
    nrow(title_row) > 0 && trimws(title_row$text[1]) == "Early Staging"
  }, seq_len(length(doc2)))

  expect_length(early_staging_slides, 2)

  for (i in early_staging_slides) {
    doc2 <- officer::on_slide(doc2, index = i)
    s <- officer::slide_summary(doc2)
    body_row <- s[s$type == "body", , drop = FALSE]
    has_subtitle <- nrow(body_row) > 0 && nzchar(trimws(body_row$text[1]))

    if (identical(layout_names[i], "Section Header")) {
      expect_true(has_subtitle, info = paste("slide", i, "(Section Header) should have the subtitle"))
      expect_equal(trimws(body_row$text[1]), paste(get_section_subtitles("lung")[["Early Staging"]], collapse = ""))
    } else {
      expect_false(has_subtitle, info = paste("slide", i, "(", layout_names[i], ") should stay untouched"))
    }
  }
})
