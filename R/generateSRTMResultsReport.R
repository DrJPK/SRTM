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
#' @param work_dir Optional directory to use as the Quarto working/output dir.
#'   Defaults to a temporary directory; on shared servers you may prefer a
#'   persistent folder under your home directory.
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
                                      format       = c("pdf", "docx", "tex", "html"),
                                      plot_palette = c("simple","pastel","modern","colourblind","greys"),
                                      paper_size   = c("A4","A4-landscape","A5","letter"),
                                      dpi          = 300,
                                      margin       = "1.5cm",
                                      pdf_paginate = TRUE,
                                      work_dir     = NULL,
                                      debug        = FALSE) {

  format     <- rlang::arg_match(format)
  plot_palette <- rlang::arg_match(plot_palette)
  paper_size <- rlang::arg_match(paper_size)

  if (!inherits(results, "srtm_analysis")) {
    rlang::abort("`results` must be an object of class 'srtm_analysis'.",
                 class = "srtm_generate_bad_results")
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    rlang::abort(
      "The 'quarto' package is required. Install it with install.packages('quarto') and ensure Quarto itself is installed.",
      class = "srtm_generate_no_quarto"
    )
  }

  # Locate template in installed package
  template_dir  <- system.file("report-templates", package = "SRTM")
  template_path <- file.path(template_dir, "srtm_results.qmd")

  if (!nzchar(template_dir) || !file.exists(template_path)) {
    rlang::abort(
      paste0(
        "Could not find 'srtm_results.qmd' in the SRTM package.\n",
        "Checked path: ", template_path
      ),
      class = "srtm_generate_no_template"
    )
  }

  # Work directory: tempdir() by default, but allow override (e.g. ~/srtm_reports)
  if (is.null(work_dir)) {
    work_dir <- file.path(tempdir(), paste0("srtm_report_", as.integer(Sys.time())))
  }
  work_dir <- normalizePath(work_dir, mustWork = FALSE)

  if (!dir.exists(work_dir)) {
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!file.access(work_dir, 2) == 0) {
    rlang::abort(
      paste0("Working directory '", work_dir, "' is not writable."),
      class = "srtm_generate_unwritable_dir"
    )
  }

  if (debug) {
    rlang::inform(
      paste0(
        "generateSRTMResultsReport(): using work_dir = '", work_dir,
        "', template_path = '", template_path, "'."
      ),
      class = "srtm_generate_debug"
    )
  }

  # Build report object once
  report_obj <- report(
    results       = results,
    alpha         = results$settings$alpha %||% 0.05,
    plot_palette  = plot_palette,
    debug         = debug
  )

  # Decide extension
  ext <- switch(format,
                pdf  = "pdf",
                docx = "docx",
                tex  = "tex",
                html = "html")

  output_file <- paste0(filename, ".", ext)
  expected_path <- file.path(work_dir, output_file)

  # Quarto params passed through
  q_params <- list(
    report_obj = report_obj,
    alpha      = report_obj$settings$alpha %||% 0.05,
    palette    = plot_palette,
    debug      = debug,
    paper_size = paper_size,
    dpi        = dpi,
    margin     = margin,
    paginate   = pdf_paginate
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(work_dir)

  # Actually call Quarto
  q_result <- tryCatch(
    quarto::quarto_render(
      input       = template_path,
      output_format = format,
      output_file = output_file,
      #output_dir  = work_dir,
      execute_params = q_params,
      quiet       = !debug
    ),
    error = function(e) {
      rlang::abort(
        paste0(
          "Quarto rendering failed: ", conditionMessage(e),
          if (format == "pdf") "\nCheck that a LaTeX distribution is installed (e.g., tinytex::install_tinytex())." else ""
        ),
        class = "srtm_generate_quarto_error"
      )
    }
  )

  if (debug) {
    rlang::inform(
      paste0("quarto_render() returned: ", capture.output(str(q_result))),
      class = "srtm_generate_debug"
    )
  }

  expected_path <- file.path(work_dir, output_file)

  if (!file.exists(expected_path)) {
    rlang::abort(
      paste0(
        "Expected output file '", expected_path,
        "' was not created by Quarto.\n",
        "On RStudio Server, this is often due to PDF engine / LaTeX issues or a mis-specified output_dir.\n",
        "Try: quarto::quarto_check(), and tinytex::install_tinytex() if you're missing LaTeX."
      ),
      class = "srtm_generate_no_output"
    )
  }

  invisible(expected_path)
}
