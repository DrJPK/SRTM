#' Print method for `srtm_group_suggestion` objects
#'
#' @description
#' Provides a compact, human-readable summary of the results returned by
#' [findGroups()]. The printout includes the time variable used, the primary
#' bandwidth settings, the detected local minima from the kernel density
#' estimate, the suggested and final number of groups, and a shortened view
#' of the bandwidth-sensitivity diagnostic.
#'
#' @param x An object of class `"srtm_group_suggestion"`, typically created by
#'   [findGroups()].
#' @param ... Additional arguments passed to or from other methods (currently
#'   ignored).
#'
#' @details
#' The summary includes:
#'
#' \itemize{
#'   \item The time variable used for group identification.
#'   \item The KDE bandwidth (`bw`) and main adjustment factor (`adjust`).
#'   \item The detected minima of the density curve (if any), used as candidate
#'         group boundaries.
#'   \item The suggested number of groups based on the KDE minima.
#'   \item The final number of groups after any interactive user override.
#'   \item A preview (first few rows) of the bandwidth-sensitivity diagnostic
#'         table stored in `x$bw_diagnostic`.
#' }
#'
#' This method is intended to provide an informative and concise overview when
#' inspecting a `srtm_group_suggestion` object interactively at the console.
#'
#' @return
#' The object `x`, invisibly (as is conventional for print methods).
#'
#' @seealso
#' [findGroups()] for creating a group suggestion object,
#' [assignGroups()] for applying group boundaries.
#'
#' @export
#'
print.srtm_group_suggestion <- function(x, ...) {
  cat("<srtm_group_suggestion>\n")
  cat("  Time variable       : ", x$time_var, "\n", sep = "")
  cat("  Bandwidth (bw)      : ", x$bw, "\n", sep = "")
  cat("  Adjust (main)       : ", x$adjust, "\n", sep = "")
  cat("  Suggested nGroups   : ", x$suggested_nGroups, "\n", sep = "")
  cat("  Final nGroups       : ", x$nGroups, "\n", sep = "")

  n_minima <- if (is.null(x$minima_x)) 0L else length(x$minima_x)
  if (n_minima > 0) {
    cat("  Minima (main adjust): ",
        paste(round(x$minima_x, 3), collapse = ", "),
        "\n", sep = "")
  } else {
    cat("  Minima (main adjust): none detected\n")
  }

  # brief bandwidth diagnostic summary if present
  if (!is.null(x$bw_diagnostic)) {
    bd <- x$bw_diagnostic
    cat("  Bandwidth diagnostic (first few rows):\n")
    # show only a small subset to avoid flooding the console
    bd_print <- utils::head(bd, 5)
    print(bd_print)
  }

  invisible(x)
}
