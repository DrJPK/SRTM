#' Print method for SRTM analysis objects
#'
#' @description
#' Compact, human-readable summary of an \code{"srtm_analysis"} object
#' produced by [SRTMAnalyse()]. Shows key settings, the number of cases,
#' and a brief overview of group combinations.
#'
#' @param x An object of class \code{"srtm_analysis"}.
#' @param ... Ignored, included for method compatibility.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @export
print.srtm_analysis <- function(x, ...) {
  if (!inherits(x, "srtm_analysis")) {
    rlang::abort(
      "`x` must be an object of class 'srtm_analysis'.",
      class = "srtm_print_bad_object"
    )
  }

  data     <- x$data
  settings <- x$settings %||% list()
  comps    <- x$Comparisons
  gsum     <- x$GroupSummary

  n_rows <- if (!is.null(data)) nrow(data) else NA_integer_
  n_base <- if (!is.null(data) && "baseGroup" %in% names(data))
    length(unique(data$baseGroup)) else NA_integer_
  n_traj <- if (!is.null(data) && "trajGroup" %in% names(data))
    length(unique(data$trajGroup)) else NA_integer_

  cat("<SRTM Analysis>\n")
  cat("  Number of cases   : ", n_rows, "\n", sep = "")
  cat("  Base groups       : ", n_base, "\n", sep = "")
  cat("  Trajectory groups : ", n_traj, "\n", sep = "")

  if (length(settings) > 0L) {
    y0  <- settings$y0  %||% "y0"
    y1  <- settings$y1  %||% "y1"
    y2  <- settings$y2  %||% "y2"
    g1  <- settings$group_first %||% NA_character_
    t01 <- settings$time01 %||% NA_real_
    t12 <- settings$time12 %||% NA_real_

    cat("  Time variables    : ", y0, " -> ", y1, " -> ", y2, "\n", sep = "")
    cat("  Time intervals    : dt01 = ", t01, ", dt12 = ", t12, "\n", sep = "")
    cat("  Grouping order    : ", g1, " first\n", sep = "")
  }

  if (!is.null(gsum) && nrow(gsum) > 0L) {
    cat("\n  Group summary (first 6 rows):\n")
    print(utils::head(gsum, 6))
  }

  if (!is.null(comps) && nrow(comps) > 0L) {
    cat("\n  Comparisons (first 6 rows):\n")
    print(utils::head(comps, 6))
  }

  invisible(x)
}


#' Summary method for SRTM analysis objects
#'
#' @description
#' Produces a merged summary table of group-wise outcomes and statistical
#' comparisons for an \code{"srtm_analysis"} object. This combines the
#' group-level summaries (from [summariseGroupOutcomes()]) with the
#' t-test-style comparisons (from [compareOutcomes()]) where possible.
#'
#' @param object An object of class \code{"srtm_analysis"}.
#' @param sort_by Character string indicating how to sort the summary.
#'   One of \code{"p"} (default, ascending p-value),
#'   \code{"abs_diff"} (descending absolute difference between observed and
#'   expected means), or \code{"none"} for no additional sorting.
#' @param ... Ignored, included for method compatibility.
#'
#' @return
#' A tibble with one row per Base × Trajectory group combination, including:
#' \itemize{
#'   \item grouping variables (`baseGroup`, `trajGroup`)
#'   \item counts and proportions
#'   \item mean observed and expected outcomes and their difference
#'   \item standard deviation and standard error of the differences
#'   \item (if available) t-value, p-value, and degrees of freedom.
#' }
#' The tibble is printed and also returned invisibly.
#'
#' @export
summary.srtm_analysis <- function(object,
                                  sort_by = c("p", "abs_diff", "none"),
                                  ...) {
  if (!inherits(object, "srtm_analysis")) {
    rlang::abort(
      "`object` must be an object of class 'srtm_analysis'.",
      class = "srtm_summary_bad_object"
    )
  }

  sort_by <- rlang::arg_match(sort_by)

  gsum <- object$GroupSummary
  comps <- object$Comparisons

  if (is.null(gsum) || nrow(gsum) == 0L) {
    rlang::warn(
      "No GroupSummary found in 'srtm_analysis' object.",
      class = "srtm_summary_no_groups"
    )
    return(invisible(gsum))
  }

  # Join comparisons if available
  if (!is.null(comps) && nrow(comps) > 0L) {
    # assume common keys baseGroup & trajGroup
    full <- dplyr::left_join(
      gsum,
      comps,
      by = c("baseGroup", "trajGroup")
    )
  } else {
    full <- gsum
  }

  # Sorting
  if (sort_by == "p" && "p" %in% names(full)) {
    full <- full %>%
      dplyr::arrange(.data$p)
  } else if (sort_by == "abs_diff") {
    # prefer mean_diff if present, else diff if present
    if ("mean_diff" %in% names(full)) {
      full <- full %>%
        dplyr::arrange(dplyr::desc(abs(.data$mean_diff)))
    } else if ("diff" %in% names(full)) {
      full <- full %>%
        dplyr::arrange(dplyr::desc(abs(.data$diff)))
    }
  }

  cat("<Summary of SRTM Analysis>\n")
  if ("pretty_p" %in% names(full)) {
    full_to_print <- full %>%
      dplyr::mutate(p = pretty_p) %>%
      dplyr::select(-pretty_p)
    print(full_to_print)
  } else {
    print(full)
  }

  invisible(full)
}

#' Plot method for SRTM analysis objects
#'
#' @description
#' Produces a panel plot of observed and expected trajectories for an
#' \code{"srtm_analysis"} object, using [plotSRTMPanel()] under the hood.
#' By default, the plot is faceted by baseline group and trajectory group.
#'
#' @param x An object of class \code{"srtm_analysis"}.
#' @param facet Logical. If \code{TRUE} (default), the panel plot is
#'   faceted, typically by baseline and trajectory groups (depending on
#'   how [plotSRTMPanel()] is implemented).
#' @param palette Optional character string specifying the plotting palette
#'   to use (e.g. \code{"simple"}, \code{"pastel"}, \code{"modern"},
#'   \code{"colourblind"}, \code{"greys"}). If \code{NULL}, the function
#'   attempts to use \code{x$settings$palette} if present, otherwise
#'   defaults to \code{"simple"}.
#' @param ... Additional arguments passed on to [plotSRTMPanel()].
#'
#' @return
#' The resulting ggplot object is printed and returned invisibly.
#'
#' @export
plot.srtm_analysis <- function(x,
                               facet   = TRUE,
                               palette = NULL,
                               ...) {
  if (!inherits(x, "srtm_analysis")) {
    rlang::abort(
      "`x` must be an object of class 'srtm_analysis'.",
      class = "srtm_plot_bad_object"
    )
  }

  if (is.null(x$data)) {
    rlang::warn(
      "No `data` component found in 'srtm_analysis' object; nothing to plot.",
      class = "srtm_plot_no_data"
    )
    return(invisible(NULL))
  }

  # decide which palette to use
  palette_to_use <- if (!is.null(palette)) {
    palette
  } else if (!is.null(x$settings$palette)) {
    x$settings$palette
  } else {
    "simple"
  }

  plt <- plotSRTMPanel(
    data    = x$data,
    facet   = facet,
    palette = palette_to_use,
    ...
  )

  print(plt)
  invisible(plt)
}

