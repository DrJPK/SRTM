#' Generate a rendered SRTM results report via Quarto
#'
#' @description
#' Wrapper around [quarto::quarto_render()] that takes an
#' \code{"srtm_analysis"} object, feeds it (via RDS) into the
#' \code{srtm_results.qmd} template, and produces a report in one of
#' several formats. Figure dimensions are chosen automatically from the
#' requested paper size, margin, and DPI so that plots occupy
#' approximately 95% of the usable page width in PDF/Word outputs, and
#' are comfortably readable on a typical laptop screen for HTML.
#'
#' @param results An object of class \code{"srtm_analysis"} as returned
#'   by [SRTMAnalyse()].
#' @param filename Output path for the final artefact. If missing, a
#'   default name is constructed based on \code{format}. For HTML, if
#'   \code{filename} does not end in \code{".zip"}, the suffix
#'   \code{".zip"} is appended (the zip contains \code{.html} plus any
#'   supporting files).
#' @param format Output format. One of \code{"pdf"}, \code{"docx"},
#'   \code{"tex"}, or \code{"html"}.
#' @param paper_size Page size for PDF/Word outputs. One of
#'   \code{"A4"}, \code{"A4-landscape"}, \code{"A5"}, or
#'   \code{"letter"}. Ignored for HTML except insofar as it informs
#'   sensible default figure heights.
#' @param dpi Plot resolution (dots per inch). Defaults to \code{300}.
#'   Passed through to the Quarto template and ultimately to
#'   \code{knitr} chunk options.
#' @param margin Page margin specification used to compute the usable
#'   figure width. May be a single numeric (interpreted as centimetres)
#'   or a character string such as \code{"1.5cm"}, \code{"20mm"}, or
#'   \code{"0.7in"}. The effective figure width is set to 95\% of
#'   \code{page_width - 2 * margin}.
#' @param pdf_paginate Logical; if \code{TRUE} and \code{format} is one
#'   of \code{"pdf"} or \code{"tex"}, a pagination flag is passed to
#'   the Quarto template so that page breaks can be inserted between
#'   per-group sections (actual page-break behaviour is controlled
#'   inside the QMD).
#' @param plot_palette Character palette name passed through to
#'   plotting helpers (e.g. [setPlotPalette()], [plotSRTMPanel()],
#'   [plotSRTMPanelSummary()]). One of \code{"simple"}, \code{"pastel"},
#'   \code{"modern"}, \code{"colourblind"}, or \code{"greys"}.
#' @param debug Logical; if \code{TRUE}, emits additional diagnostic
#'   messages and leaves intermediate files in the temporary working
#'   directory (useful when developing the template).
#'
#' @details
#' For \strong{PDF}, \strong{DOCX}, and \strong{TEX} outputs, the
#' function:
#' \enumerate{
#'   \item Maps \code{paper_size} to a page width in inches (A4, A5,
#'         letter; portrait/landscape as appropriate).
#'   \item Parses \code{margin} and converts it to inches.
#'   \item Computes a figure width as
#'         \eqn{0.95 \times (page\_width - 2 \times margin)}.
#'   \item Chooses a figure height as a moderate aspect ratio multiple
#'         of the width (approximately 0.6 of width).
#' }
#'
#' For \strong{HTML}, the function chooses a fixed figure width/height
#' (roughly 7.5 × 4.5 inches) intended to be comfortably readable on a
#' 1366×768 laptop screen. All HTML artefacts are rendered into a
#' temporary directory, then zipped; the resulting \code{.zip} is
#' copied to \code{filename}.
#'
#' For \strong{TEX}, the Quarto template is rendered as PDF with
#' \code{keep-tex: true} assumed in the YAML. The function then locates
#' the resulting \code{.tex} file in the temporary directory and copies
#' it to \code{filename} (falling back to the PDF if no \code{.tex}
#' file can be found).
#'
#' @return Invisibly, the normalised path to the final output file
#'   (PDF, DOCX, TEX, or ZIP containing HTML + support files).
#'
#' @importFrom rlang abort inform
#' @importFrom glue glue
#' @importFrom tools file_path_sans_ext
#' @importFrom utils zip
#' @importFrom stats setNames
#' @importFrom quarto quarto_render
#'
#' @export
generateSRTMResultsReport <- function(results,
                                      filename,
                                      format      = c("pdf", "docx", "tex", "html"),
                                      paper_size  = c("A4", "A4-landscape", "A5", "letter"),
                                      dpi         = 300,
                                      margin      = "1.5cm",
                                      pdf_paginate = TRUE,
                                      plot_palette = c("simple", "pastel", "modern", "colourblind", "greys"),
                                      debug       = FALSE) {

  # ---- basic checks --------------------------------------------------------
  if (!inherits(results, "srtm_analysis")) {
    rlang::abort(
      "`results` must be an object of class 'srtm_analysis' (output of SRTMAnalyse()).",
      class = "srtm_generate_bad_results"
    )
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    rlang::abort(
      "The 'quarto' package is required to render reports. Please install Quarto and the 'quarto' R package.",
      class = "srtm_generate_no_quarto"
    )
  }

  format     <- rlang::arg_match(format)
  paper_size <- rlang::arg_match(paper_size)
  plot_palette <- rlang::arg_match(plot_palette)

  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    rlang::abort("`dpi` must be a single positive numeric value.", class = "srtm_generate_bad_dpi")
  }

  if (!is.logical(pdf_paginate) || length(pdf_paginate) != 1L || is.na(pdf_paginate)) {
    rlang::abort("`pdf_paginate` must be TRUE or FALSE.", class = "srtm_generate_bad_paginate")
  }

  if (!is.logical(debug) || length(debug) != 1L || is.na(debug)) {
    rlang::abort("`debug` must be TRUE or FALSE.", class = "srtm_generate_bad_debug")
  }

  # ---- helper: parse margin to inches --------------------------------------
  margin_to_inches <- function(x) {
    if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
      # interpret bare numerics as centimetres
      return(as.numeric(x) / 2.54)
    }

    if (!is.character(x) || length(x) != 1L) {
      rlang::abort("`margin` must be a single numeric or a length-1 character (e.g. '1.5cm').",
                   class = "srtm_generate_bad_margin")
    }

    m <- trimws(x)
    m_re <- "^\\s*([0-9]*\\.?[0-9]+)\\s*(cm|mm|in)?\\s*$"
    if (!grepl(m_re, m)) {
      rlang::abort(
        glue::glue("Could not parse `margin` = '{m}'. Use e.g. '1.5cm', '20mm', or '0.7in'."),
        class = "srtm_generate_bad_margin"
      )
    }
    parts <- sub(m_re, "\\1|\\2", m)
    num   <- as.numeric(sub("\\|.*$", "", parts))
    unit  <- sub("^.*\\|", "", parts)
    if (!nzchar(unit)) unit <- "cm"

    if (unit == "cm") {
      num / 2.54
    } else if (unit == "mm") {
      num / 25.4
    } else if (unit == "in") {
      num
    } else {
      rlang::abort(
        glue::glue("Unknown margin unit '{unit}'. Use 'cm', 'mm', or 'in'."),
        class = "srtm_generate_bad_margin_unit"
      )
    }
  }

  # ---- compute figure size -------------------------------------------------
  margin_in <- margin_to_inches(margin)

  page_width_in <- switch(
    paper_size,
    "A4"           = 21 / 2.54,
    "A4-landscape" = 29.7 / 2.54,
    "A5"           = 14.8 / 2.54,
    "letter"       = 8.5,
    8.27  # default-ish
  )

  if (format %in% c("pdf", "docx", "tex")) {
    usable_width <- max(page_width_in - 2 * margin_in, 1)  # avoid degenerate
    fig_width  <- 0.95 * usable_width
    fig_height <- fig_width * 0.6  # moderate aspect ratio
  } else {
    # HTML: choose something that looks good on 1366×768
    fig_width  <- 7.5
    fig_height <- 4.5
  }

  if (debug) {
    rlang::inform(
      glue::glue(
        "generateSRTMResultsReport(): format = '{format}', paper_size = '{paper_size}', ",
        "fig_width = {round(fig_width, 2)} in, fig_height = {round(fig_height, 2)} in, dpi = {dpi}."
      ),
      class = "srtm_generate_debug"
    )
  }

  # ---- locate template and set up temp dir ---------------------------------
  template_path <- system.file("srtm_results.qmd", package = "SRTM")
  if (!nzchar(template_path) || !file.exists(template_path)) {
    rlang::abort(
      "Could not find 'srtm_results.qmd' in the SRTM package. Check that the template is installed.",
      class = "srtm_generate_no_template"
    )
  }

  work_dir <- tempfile("srtm_report_")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  # Save results object as RDS for the QMD to load
  results_rds <- file.path(work_dir, "srtm_results.rds")
  saveRDS(results, results_rds)

  # Decide base name + final filename
  if (missing(filename) || is.null(filename) || !nzchar(filename)) {
    base <- glue::glue("SRTM-results-{format}-{Sys.Date()}")
    filename <- paste0(base, switch(format,
                                    pdf  = ".pdf",
                                    docx = ".docx",
                                    tex  = ".tex",
                                    html = ".zip"))
  }

  # normalise user-supplied path
  filename <- normalizePath(filename, winslash = "/", mustWork = FALSE)
  out_dir  <- dirname(filename)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  base_name <- tools::file_path_sans_ext(basename(filename))

  # ---- prepare Quarto render args ------------------------------------------
  # Quarto format string
  q_format <- switch(
    format,
    pdf  = "pdf",
    docx = "docx",
    tex  = "pdf",   # rely on keep-tex in template
    html = "html"
  )

  # We always render into the temp working directory
  out_file_stub <- paste0(base_name, ".", if (format == "html") "html" else q_format)
  render_args <- list(
    input        = template_path,
    output_file  = out_file_stub,
    output_format = q_format,
    execute_params = list(
      results_rds   = results_rds,
      fig_width     = fig_width,
      fig_height    = fig_height,
      dpi           = dpi,
      plot_palette  = plot_palette,
      pdf_paginate  = pdf_paginate
    ),
    execute_dir  = work_dir,
    quiet        = !debug
  )

  # ---- render via Quarto ---------------------------------------------------
  if (debug) {
    rlang::inform(
      glue::glue(
        "generateSRTMResultsReport(): calling quarto_render() with format = '{q_format}'."
      ),
      class = "srtm_generate_debug"
    )
  }

  do.call(quarto::quarto_render, render_args)

  # ---- collect output depending on format ----------------------------------
  result_path <- NULL

  if (format %in% c("pdf", "docx")) {
    rendered <- file.path(work_dir, out_file_stub)
    if (!file.exists(rendered)) {
      rlang::abort(
        glue::glue("Expected output file '{rendered}' was not created by Quarto."),
        class = "srtm_generate_missing_output"
      )
    }
    result_path <- filename
    file.copy(rendered, result_path, overwrite = TRUE)

  } else if (format == "tex") {
    # Look for corresponding .tex (assumes keep-tex: true in template)
    tex_candidate <- file.path(work_dir, paste0(base_name, ".tex"))
    if (file.exists(tex_candidate)) {
      tex_src <- tex_candidate
    } else {
      tex_files <- list.files(work_dir, pattern = "\\.tex$", full.names = TRUE, recursive = TRUE)
      if (length(tex_files) == 0L) {
        rlang::warn(
          "No .tex file was found; returning the rendered PDF instead.",
          class = "srtm_generate_no_tex"
        )
        tex_files <- file.path(work_dir, out_file_stub)  # PDF fallback
      }
      tex_src <- tex_files[[1L]]
    }

    dest <- filename
    if (!grepl("\\.tex$", dest, ignore.case = TRUE)) {
      dest <- paste0(tools::file_path_sans_ext(dest), ".tex")
    }
    result_path <- dest
    file.copy(tex_src, dest, overwrite = TRUE)

  } else if (format == "html") {
    # HTML + support files zipped
    html_file <- file.path(work_dir, out_file_stub)
    if (!file.exists(html_file)) {
      rlang::abort(
        glue::glue("Expected HTML file '{html_file}' was not created by Quarto."),
        class = "srtm_generate_missing_html"
      )
    }

    # Quarto usually creates a *_files directory; include it if present
    html_stub <- tools::file_path_sans_ext(basename(html_file))
    support_dir <- file.path(work_dir, paste0(html_stub, "_files"))

    files_to_zip <- basename(html_file)
    if (dir.exists(support_dir)) {
      # include all files inside support dir
      support_rel <- file.path(basename(support_dir),
                               list.files(support_dir, recursive = TRUE))
      files_to_zip <- c(files_to_zip, support_rel)
    }

    # create zip inside work_dir then copy to final location
    zip_name <- file.path(work_dir, paste0(base_name, "_html_bundle.zip"))

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(work_dir)

    utils::zip(zipfile = basename(zip_name), files = files_to_zip)

    # decide final filename (ensure .zip)
    dest <- filename
    if (!grepl("\\.zip$", dest, ignore.case = TRUE)) {
      dest <- paste0(tools::file_path_sans_ext(dest), ".zip")
    }
    file.copy(zip_name, dest, overwrite = TRUE)
    result_path <- dest
  }

  if (!debug) {
    # best-effort clean-up of temp dir (safe if fails)
    try(unlink(work_dir, recursive = TRUE, force = TRUE), silent = TRUE)
  } else {
    rlang::inform(
      glue::glue("Debug mode: intermediate files left in '{work_dir}'."),
      class = "srtm_generate_debug"
    )
  }

  invisible(normalizePath(result_path, winslash = "/"))
}
