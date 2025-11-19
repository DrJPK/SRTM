#' Save an SRTM plot with sensible defaults for different use cases
#'
#' @description
#' Convenience wrapper around [ggplot2::ggsave()] that:
#' \itemize{
#'   \item uses the last plot if `plot` is not supplied (like `ggsave()`),
#'   \item chooses sensible sizes/resolutions based on `format`:
#'     \itemize{
#'       \item `"journal"` – max ~16 cm wide, 300 dpi, saves PNG, SVG, and TIFF;
#'       \item `"presentation"` – slide-style PNG suitable for seminar rooms;
#'       \item `"fullScreen"` – PNG sized for ~1920 × 1080 screens.
#'     }
#'   \item writes files into `dir` (default `"outputs"`), creating it if needed,
#'   \item appends `_<format>` to `file_stem` for the preset modes.
#' }
#'
#' If `format` is `NULL` or missing, `plotSave()` falls back to a manual mode:
#' you must provide `width`, `height`, and `dpi`, and `file_stem` must include
#' an extension (e.g., `"my_figure.png"`). In this case a single file is saved.
#'
#' @param file_stem Character stem for the output filename (no extension for
#'   preset formats). For manual mode (when `format` is `NULL`), this should
#'   include the desired extension (e.g. `"myplot.png"`).
#' @param plot A ggplot or grid object to save. If `NULL`, uses
#'   [ggplot2::last_plot()].
#' @param dir Directory to save into. Defaults to `"outputs"`.
#' @param format One of `"journal"`, `"presentation"`, `"fullScreen"`, or
#'   `NULL`. If `NULL`, manual mode is used.
#' @param width Optional numeric width. If supplied, overrides the default
#'   width for the chosen format.
#' @param height Optional numeric height. If supplied, overrides the default
#'   height for the chosen format.
#' @param dpi Optional numeric DPI. If supplied, overrides the default DPI
#'   for the chosen format.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return Invisibly returns a character vector of file paths that were written.
#'
#' @export
plotSave <- function(file_stem,
                     plot   = NULL,
                     dir    = "outputs",
                     format = c("journal", "presentation", "fullScreen"),
                     width  = NULL,
                     height = NULL,
                     dpi    = NULL,
                     ...) {

  # small helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Decide if we are in preset mode or manual mode
  preset_mode <- !missing(format) && !is.null(format)

  if (preset_mode) {
    format <- rlang::arg_match(format)
  } else {
    format <- NULL
  }

  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  if (is.null(plot)) {
    rlang::abort(
      "No plot supplied and no last plot available.",
      class = "srtm_plotSave_no_plot"
    )
  }

  # Ensure output directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  saved_files <- character(0)

  # ---------------------------------------------------------------------------
  # PRESET MODES
  # ---------------------------------------------------------------------------
  if (!is.null(format)) {

    if (identical(format, "journal")) {
      # Defaults for journal: max ~16 cm wide, 300 dpi
      width_cm  <- width  %||% 16
      height_cm <- height %||% 9
      dpi_use   <- dpi    %||% 300

      base <- file.path(dir, paste0(file_stem, "_journal"))

      # PNG
      png_file <- paste0(base, ".png")
      ggplot2::ggsave(
        filename = png_file,
        plot     = plot,
        width    = width_cm,
        height   = height_cm,
        units    = "cm",
        dpi      = dpi_use,
        ...
      )
      saved_files <- c(saved_files, png_file)

      # SVG
      svg_file <- paste0(base, ".svg")
      ggplot2::ggsave(
        filename = svg_file,
        plot     = plot,
        width    = width_cm,
        height   = height_cm,
        units    = "cm",
        dpi      = dpi_use,
        device   = "svg",
        ...
      )
      saved_files <- c(saved_files, svg_file)

      # TIFF
      tiff_file <- paste0(base, ".tiff")
      ggplot2::ggsave(
        filename = tiff_file,
        plot     = plot,
        width    = width_cm,
        height   = height_cm,
        units    = "cm",
        dpi      = dpi_use,
        device   = "tiff",
        ...
      )
      saved_files <- c(saved_files, tiff_file)

    } else if (identical(format, "presentation")) {
      # Defaults for presentation: slide-style PNG
      # Target ~1600x900 px at ~150 dpi
      dpi_use <- dpi %||% 150
      width_in  <- width  %||% (1600 / dpi_use)
      height_in <- height %||% (900  / dpi_use)

      out_file <- file.path(dir, paste0(file_stem, "_presentation.png"))
      ggplot2::ggsave(
        filename = out_file,
        plot     = plot,
        width    = width_in,
        height   = height_in,
        units    = "in",
        dpi      = dpi_use,
        ...
      )
      saved_files <- c(saved_files, out_file)

    } else if (identical(format, "fullScreen")) {
      # Defaults for fullScreen: PNG sized for ~1920x1080 or better
      dpi_use <- dpi %||% 96
      width_in  <- width  %||% (1920 / dpi_use)
      height_in <- height %||% (1080 / dpi_use)

      out_file <- file.path(dir, paste0(file_stem, "_fullScreen.png"))
      ggplot2::ggsave(
        filename = out_file,
        plot     = plot,
        width    = width_in,
        height   = height_in,
        units    = "in",
        dpi      = dpi_use,
        ...
      )
      saved_files <- c(saved_files, out_file)
    }

  } else {
    # -------------------------------------------------------------------------
    # MANUAL MODE (no format): require width, height, dpi and extension
    # -------------------------------------------------------------------------
    if (is.null(width) || is.null(height) || is.null(dpi)) {
      rlang::abort(
        "When `format` is NULL, you must supply `width`, `height`, and `dpi`.",
        class = "srtm_plotSave_manual_missing"
      )
    }

    # file_stem should include extension in this mode
    out_file <- file.path(dir, file_stem)

    if (!grepl("\\.[A-Za-z0-9]+$", out_file)) {
      rlang::warn(
        "In manual mode, `file_stem` should include a file extension (e.g. '.png').",
        class = "srtm_plotSave_no_ext"
      )
    }

    ggplot2::ggsave(
      filename = out_file,
      plot     = plot,
      width    = width,
      height   = height,
      dpi      = dpi,
      ...
    )
    saved_files <- c(saved_files, out_file)
  }

  invisible(saved_files)
}
