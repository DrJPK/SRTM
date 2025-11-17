#' Set SRTM plotting palette
#'
#' @description
#' Returns a list containing colour, shape, and linetype settings suitable
#' for SRTM visualisations. The palette controls the colours used mainly for
#' base groups, while shapes and linetypes are fixed conventions:
#' observed trajectories use solid lines, expected trajectories use dotted
#' lines; trajectory groups are distinguished by point shapes.
#'
#' @param palette Character string indicating which palette to use.
#'   One of \code{"simple"}, \code{"pastel"}, \code{"modern"},
#'   \code{"colourblind"}, or \code{"greys"}. Defaults to \code{"simple"}.
#'
#' @details
#' Each palette defines five colours chosen to work well on screen and
#' in print. Shapes and linetypes are currently fixed:
#' \itemize{
#'   \item \code{linetypes}: \code{observed = "solid"}, \code{expected = "dotted"}.
#'   \item \code{shapes}: a vector \code{c(16, 17, 15, 3, 7)} used for trajectory groups.
#' }
#'
#' @return
#' A list with class \code{"srtm_palette"} containing:
#' \itemize{
#'   \item \code{palette} — the palette name.
#'   \item \code{colours} — named character vector of hex colours.
#'   \item \code{shapes} — integer vector of point shapes.
#'   \item \code{linetypes} — named character vector for linetypes.
#' }
#'
#' @examples
#' pal <- setPlotPalette("colourblind")
#' pal$colours
#'
#' @export
setPlotPalette <- function(palette = c("simple", "pastel", "modern", "colourblind", "greys")) {
  palette <- rlang::arg_match(palette)

  colours <- switch(
    palette,
    "simple" = c(
      A = "#1f77b4",  # blue
      B = "#ff7f0e",  # orange
      C = "#2ca02c",  # green
      D = "#9467bd",  # purple
      E = "#d62728"   # red
    ),
    "pastel" = c(
      A = "#a6cee3",  # pastel blue
      B = "#b2df8a",  # pastel green
      C = "#fdbf6f",  # pastel orange
      D = "#cab2d6",  # pastel purple
      E = "#fb9a99"   # pastel red
    ),
    "modern" = c(
      A = "#1b9e77",  # teal
      B = "#d95f02",  # orange
      C = "#7570b3",  # indigo
      D = "#e7298a",  # magenta
      E = "#66a61e"   # lime/green
    ),
    "colourblind" = c(
      # Okabe–Ito style palette
      A = "#0072B2",  # blue
      B = "#E69F00",  # orange
      C = "#009E73",  # green
      D = "#D55E00",  # vermillion
      E = "#CC79A7"   # reddish purple
    ),
    "greys" = c(
      A = "#222222",
      B = "#555555",
      C = "#888888",
      D = "#BBBBBB",
      E = "#DDDDDD"
    )
  )

  shapes <- c(16, 17, 15, 3, 7)  # filled circle, triangle, square, plus, cross

  linetypes <- c(
    observed = "solid",
    expected = "dotted"
  )

  res <- list(
    palette   = palette,
    colours   = colours,
    shapes    = shapes,
    linetypes = linetypes
  )

  class(res) <- c("srtm_palette", class(res))
  res
}
