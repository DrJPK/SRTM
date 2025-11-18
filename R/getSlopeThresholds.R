#' Extract slope classification thresholds from SRTM objects
#'
#' @description
#' Helper to retrieve the slope classification thresholds (used for
#' semantic labelling of trajectory groups) from an SRTM object or a
#' grouping factor. These thresholds are stored as the
#' \code{"srtm_slope_thresholds"} attribute on a factor (typically the
#' \code{trajGroup} column) created by [assignGroups()] when
#' \code{time_var == "m01"} and a non-\code{"letters"} \code{label_scheme}
#' is used.
#'
#' @param x Either:
#'   \itemize{
#'     \item An \code{"srtm_analysis"} object as returned by [SRTMAnalyse()],
#'       in which case \code{group_var} is looked up in \code{x$data}, or
#'     \item A data frame/tibble containing \code{group_var}, or
#'     \item A factor/atomic vector that already carries the
#'       \code{"srtm_slope_thresholds"} attribute.
#'   }
#' @param group_var Character scalar naming the trajectory group column
#'   (defaults to \code{"trajGroup"}) when \code{x} is an
#'   \code{"srtm_analysis"} object or a data frame.
#'
#' @return
#' A list with components:
#' \itemize{
#'   \item \code{time_var} — variable name used for slope grouping (e.g. \code{"m01"}).
#'   \item \code{dt01} — time interval used for slope classification.
#'   \item \code{scale_range} — inferred outcome scale range.
#'   \item \code{steep_thresh} — threshold for "steep" slopes.
#'   \item \code{flat_thresh} — threshold for "flat" slopes.
#' }
#' If no thresholds are found, \code{NULL} is returned (invisibly) and a
#' warning may be issued.
#'
#' @export
getSlopeThresholds <- function(x, group_var = "trajGroup") {

  # figure out where the grouping factor lives
  if (inherits(x, "srtm_analysis")) {
    if (is.null(x$data)) {
      rlang::warn(
        "No `data` component found in 'srtm_analysis' object; cannot extract thresholds.",
        class = "srtm_getSlopeThresholds_no_data"
      )
      return(invisible(NULL))
    }
    if (!group_var %in% names(x$data)) {
      rlang::warn(
        glue::glue("Column `{group_var}` not found in `x$data`; cannot extract thresholds."),
        class = "srtm_getSlopeThresholds_missing_col"
      )
      return(invisible(NULL))
    }
    f <- x$data[[group_var]]

  } else if (is.data.frame(x)) {
    if (!group_var %in% names(x)) {
      rlang::warn(
        glue::glue("Column `{group_var}` not found in data; cannot extract thresholds."),
        class = "srtm_getSlopeThresholds_missing_col"
      )
      return(invisible(NULL))
    }
    f <- x[[group_var]]

  } else {
    # assume x itself is the factor/vector
    f <- x
  }

  thr <- attr(f, "srtm_slope_thresholds", exact = TRUE)

  if (is.null(thr)) {
    rlang::warn(
      "No 'srtm_slope_thresholds' attribute found; returning NULL.",
      class = "srtm_getSlopeThresholds_no_attr"
    )
    return(invisible(NULL))
  }

  thr
}
