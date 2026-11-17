# PPTX Section Subtitles
# =======================
# Adds a subtitle line to each "Section Header" divider slide in a rendered
# data pack, showing the raw dataset filter value(s) that section's figures
# were built from. Runs as a POST-PROCESSING step, after
# rmarkdown::render() (see scripts/main.R) — it cannot be done via markdown
# in the Rmd itself.
#
# Why not markdown: adding body content directly under a level-1 (#) heading
# changes pandoc's auto-detected slide level for the WHOLE document (pandoc
# picks the slide level as the shallowest heading level that has its own
# direct content). Once that happens, every level-2 (##) heading elsewhere in
# the pack stops starting its own slide and is demoted to bold inline text on
# whichever level-1 slide it falls under — silently merging dozens of slides
# into a handful. Forcing slide_level: 2 in the YAML avoids that but creates
# a NEW, separate, title-less slide for the level-1 content instead of
# putting it on the same slide as the title. Confirmed both failure modes
# with a minimal reproduction against this project's own reference_doc
# before writing this file.
#
# This works instead by using officer to write directly into the "Section
# Header" layout's existing body placeholder (confirmed present via
# officer::layout_properties(), ph_label "Text Placeholder 2", positioned
# directly under the title) on the already-rendered slide. No markdown
# structure is touched, so slide-level detection is unaffected.
#
# Matching is by slide LAYOUT, not title text alone. The Rmd reuses a
# section's exact title as the title of its own first content slide too (e.g.
# "# Early Staging" immediately followed by "## Early Staging" for the first
# chart) — title-only matching hit BOTH, because the chart slide's body holds
# an SVG image rather than text, so it read as "empty" to a text-only guard
# and got the subtitle bullet injected on top of the chart. Restricting to
# the "Section Header" layout is the direct fix: only the genuine divider
# slide uses it, regardless of what any other slide is titled.
#
# Every Source/Filter line (format_subtitle_lines(), R/load_tumour_config.R)
# separates the label from the value with a literal tab character so all
# values — single-line and multi-line alike — align in one column,
# rather than space-padding — the template's font is proportional, so
# "Source:" and "Filter:" don't render at the same width despite being the
# same character count, and the three CWT standard labels ("FDS:",
# "31-day:", "62-day:") are different widths again. A bare "\t" without an
# explicit tab stop falls back to PowerPoint's default 1-inch increments,
# which does not land every line at the same column either (the first
# line's own pre-tab text is a different width from the continuation
# lines' empty one). officer::fp_par(tabs = ...)/fp_tab() exist but are not
# serialised into the pptx XML by this officer version (confirmed by
# inspecting the generated XML directly — no <a:tabLst> was emitted), so
# add_tab_stop_to_body() injects the <a:tabLst> paragraph property directly
# via xml2 after ph_with() writes the text.

#' Get the section subtitle lines for every section title used in the template
#'
#' Each value is a character VECTOR, one element per PowerPoint paragraph
#' (see add_section_subtitles() — ph_with() renders each vector element as
#' its own line), not a single string: a "Source:\t..." line, then one or more
#' "Filter:\t..." lines, with no parentheses or pipe-delimiters run together on
#' one line.
#'
#' @param tumour_key Character string identifying the tumour type
#' @return Named list, name = the section's exact "#"-level heading text as it
#'   appears in scripts/Tumour_Data_Pack_Template.Rmd, value = a character
#'   vector of subtitle lines for that section.
get_section_subtitles <- function(tumour_key) {
  list(
    "Cancer Waiting Times" = format_cwt_filter_subtitle(tumour_key),
    # Treatment Variation now has its own real filter value (see
    # config/tumours.yml's treatment_variation field) — no longer an implicit
    # match on the tumour's display name.
    "Treatment Variation" = format_split_aware_filter_subtitle(tumour_key, "treatment_variation"),
    "Staging Completeness" = format_split_aware_filter_subtitle(tumour_key, "staging_completeness"),
    "Early Staging" = format_split_aware_filter_subtitle(tumour_key, "staging_demographics"),
    "Living With and Beyond Cancer" = format_split_aware_filter_subtitle(tumour_key, "lwbc")
  )
}


#' Tab stop position (inches) for multi-line subtitle label/value columns
#'
#' Measured against the Section Header layout's body placeholder font (24pt,
#' theme font "Aptos") so it clears the widest label ("Source:", ~1.15in at
#' that size/font) with a small margin. If a future template changes that
#' placeholder's font or size, re-check alignment on a rendered slide and
#' adjust this constant.
SUBTITLE_TAB_STOP_IN <- 1.3

#' Add an explicit tab stop to every subtitle paragraph that uses one
#'
#' format_subtitle_lines() (R/load_tumour_config.R) separates every
#' subtitle line's label from its value with a literal tab character so all
#' values land in the same column. A
#' bare tab character with no tab stop defined falls back to PowerPoint's
#' default 1-inch increments, which does not reliably align every line (see
#' R/pptx_section_subtitles.R's header comment for why). officer's
#' fp_par(tabs = ...)/fp_tab() are not serialised into pptx XML by the
#' installed officer version, so this injects the <a:tabLst> paragraph
#' property directly via xml2 instead, on only the paragraphs that actually
#' contain a tab character.
#'
#' @param doc An officer rpptx document, after ph_with() has written this
#'   slide's body text.
#' @param slide_index Integer. The slide to fix up, 1-based.
#' @param tab_stop_in Numeric. Tab stop position in inches.
#' @return Invisibly, doc. Mutates doc's slide XML in place via xml2 (the
#'   same object is also returned for pipe-friendliness).
add_tab_stop_to_body <- function(doc, slide_index, tab_stop_in = SUBTITLE_TAB_STOP_IN) {
  slide_xml <- doc$slide$get_slide(slide_index)$get()
  ns <- xml2::xml_ns(slide_xml)
  body_sp <- xml2::xml_find_first(slide_xml, ".//p:sp[.//p:ph[@type='body']]", ns = ns)
  paragraphs <- xml2::xml_find_all(body_sp, ".//a:p", ns = ns)
  pos_emu <- as.character(round(tab_stop_in * 914400))

  for (p in paragraphs) {
    if (!grepl("\t", xml2::xml_text(p), fixed = TRUE)) next
    ppr <- xml2::xml_add_child(p, "a:pPr", .where = 0)
    tab_lst <- xml2::xml_add_child(ppr, "a:tabLst")
    xml2::xml_add_child(tab_lst, "a:tab", pos = pos_emu, algn = "l")
  }

  invisible(doc)
}


#' Get the layout name used by each slide in a pptx document
#'
#' officer::slide_summary() doesn't expose which layout a slide uses;
#' officer::layout_summary() lists layout names but not per-slide. This joins
#' the document's own per-slide layout FILE (doc$slide$get_metadata()) against
#' the layout FILE -> NAME mapping (doc$slideLayouts$get_metadata()) to get a
#' name per slide, in slide order.
#'
#' @param doc An officer rpptx document (officer::read_pptx()).
#' @return Character vector, one layout name per slide, in slide order.
get_slide_layout_names <- function(doc) {
  slide_layout_files <- basename(doc$slide$get_metadata()$layout_file)
  layout_lookup <- doc$slideLayouts$get_metadata()
  layout_lookup$name[match(slide_layout_files, layout_lookup$filename)]
}


#' Inject section subtitles into a rendered data pack's Section Header slides
#'
#' For each slide using the "Section Header" layout whose title exactly
#' matches a name returned by get_section_subtitles(), sets that slide's body
#' placeholder to the corresponding subtitle lines — one PowerPoint paragraph
#' per character-vector element (see get_section_subtitles()), giving real
#' line breaks rather than one run-on line. A slide for a section this tumour
#' doesn't have (e.g. Treatment Variation when treatment_variation is null)
#' was never rendered in the first place, so its title never appears and this
#' needs no gating of its own — the lookup simply finds no match.
#'
#' The layout restriction is necessary, not just tidy: several sections reuse
#' their own exact title as the title of their first content slide too (e.g.
#' "Early Staging" names both the "# Early Staging" divider and its
#' "## Early Staging" chart slide) — matching on title alone would hit both.
#'
#' Defensive guard: a matched slide is only written to if its body
#' placeholder is currently empty. Every Section Header slide is blank in the
#' body until this function runs, so that's the expected case.
#'
#' @param pptx_path Character. Path to the rendered .pptx file, as returned
#'   by rmarkdown::render().
#' @param tumour_key Character string identifying the tumour type.
#' @return Invisibly, pptx_path. Called for the side effect of overwriting
#'   the file in place.
#' @export
add_section_subtitles <- function(pptx_path, tumour_key) {
  subtitles <- get_section_subtitles(tumour_key)

  doc <- officer::read_pptx(pptx_path)
  layout_names <- get_slide_layout_names(doc)

  for (i in seq_len(length(doc))) {
    if (!identical(layout_names[i], "Section Header")) next

    doc <- officer::on_slide(doc, index = i)
    slide_info <- officer::slide_summary(doc)

    title_row <- slide_info[slide_info$type %in% c("title", "ctrTitle"), , drop = FALSE]
    if (nrow(title_row) == 0) next

    subtitle_lines <- subtitles[[title_row$text[1]]]
    if (is.null(subtitle_lines)) next

    body_row <- slide_info[slide_info$type == "body", , drop = FALSE]
    existing_body <- if (nrow(body_row) == 0) "" else trimws(paste(body_row$text, collapse = ""))
    if (nzchar(existing_body)) {
      warning(
        "Skipping section subtitle for '", title_row$text[1], "' (slide ", i,
        "): body placeholder already has content — expected only on a genuine",
        " Section Header slide, which is blank before this function runs.",
        call. = FALSE
      )
      next
    }

    doc <- officer::ph_with(
      doc,
      value = subtitle_lines,
      location = officer::ph_location_type(type = "body")
    )
    add_tab_stop_to_body(doc, i)
  }

  print(doc, target = pptx_path)
  invisible(pptx_path)
}
