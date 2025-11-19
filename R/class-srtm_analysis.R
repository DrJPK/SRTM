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
#' Produces compact summary tables of group-wise outcomes and statistical
#' comparisons for an \code{"srtm_analysis"} object. This combines the
#' group-level summaries (from [summariseGroupOutcomes()]) with the
#' t-test-style comparisons for outcomes (from [compareOutcomes()]) and,
#' if available, slope comparisons (from [compareSlopes()]).
#'
#' Grouping is shown using `baseGroup` and `trajType` (older objects that
#' still use `trajGroup` are normalised to `trajType` for display).
#'
#' @param object An object of class \code{"srtm_analysis"}.
#' @param sort_by Character string indicating how to sort the summary.
#'   One of:
#'   \itemize{
#'     \item \code{"p"} (default): ascending p-value for outcome comparisons
#'           (falls back to slope p-values if outcome p-values are not present),
#'     \item \code{"abs_diff"}: descending absolute difference between observed
#'           and expected means (\code{mean_diff} if available, otherwise
#'           the absolute outcome difference),
#'     \item \code{"none"}: no additional sorting.
#'   }
#' @param ... Ignored, included for method compatibility.
#'
#' @return
#' A tibble with one row per Base × Trajectory type combination (the merged
#' table used to construct the printed summaries). The tibble is returned
#' invisibly.
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

  gsum          <- object$GroupSummary
  comps_outcome <- object$Comparisons
  # accept either field name for slopes
  comps_slope   <- object$Comparisons_slopes %||% object$Slope_Comparisons

  if (is.null(gsum) || nrow(gsum) == 0L) {
    rlang::warn(
      "No GroupSummary found in 'srtm_analysis' object.",
      class = "srtm_summary_no_groups"
    )
    return(invisible(gsum))
  }

  # --- normalise group column names to trajType ----------------------------
  if ("trajGroup" %in% names(gsum) && !"trajType" %in% names(gsum)) {
    gsum <- dplyr::rename(gsum, trajType = .data$trajGroup)
  }

  if (!is.null(comps_outcome) &&
      "trajGroup" %in% names(comps_outcome) &&
      !"trajType" %in% names(comps_outcome)) {
    comps_outcome <- dplyr::rename(comps_outcome, trajType = .data$trajGroup)
  }

  if (!is.null(comps_slope) &&
      "trajGroup" %in% names(comps_slope) &&
      !"trajType" %in% names(comps_slope)) {
    comps_slope <- dplyr::rename(comps_slope, trajType = .data$trajGroup)
  }

  # basic sanity: require baseGroup and trajType in GroupSummary
  required_keys <- c("baseGroup", "trajType")
  missing_keys  <- setdiff(required_keys, names(gsum))
  if (length(missing_keys) > 0L) {
    rlang::abort(
      paste0(
        "The GroupSummary component must contain columns: ",
        paste(required_keys, collapse = ", "),
        ". Missing: ",
        paste(missing_keys, collapse = ", ")
      ),
      class = "srtm_summary_missing_keys"
    )
  }

  # --- rename comparison columns to distinguish outcome vs slope -----------

  if (!is.null(comps_outcome) && nrow(comps_outcome) > 0L) {
    comps_outcome <- comps_outcome %>%
      dplyr::rename(
        t_outcome        = .data$t_value,
        diff_outcome     = .data$diff,
        p_outcome        = .data$p,
        pretty_p_outcome = .data$pretty_p,
        df_outcome       = .data$df
      )
  }

  if (!is.null(comps_slope) && nrow(comps_slope) > 0L) {
    comps_slope <- comps_slope %>%
      dplyr::rename(
        t_slope        = .data$t_value,
        diff_slope     = .data$diff,
        p_slope        = .data$p,
        pretty_p_slope = .data$pretty_p,
        df_slope       = .data$df
      )
  }

  # --- join everything together --------------------------------------------
  full <- gsum

  if (!is.null(comps_outcome) && nrow(comps_outcome) > 0L) {
    full <- dplyr::left_join(
      full,
      comps_outcome,
      by = c("baseGroup", "trajType")
    )
  }

  if (!is.null(comps_slope) && nrow(comps_slope) > 0L) {
    full <- dplyr::left_join(
      full,
      comps_slope,
      by = c("baseGroup", "trajType")
    )
  }

  # --- sorting -------------------------------------------------------------
  if (sort_by == "p") {
    if ("p_outcome" %in% names(full)) {
      full <- full %>% dplyr::arrange(.data$p_outcome)
    } else if ("p_slope" %in% names(full)) {
      full <- full %>% dplyr::arrange(.data$p_slope)
    } else if ("p" %in% names(full)) {
      full <- full %>% dplyr::arrange(.data$p)
    }
  } else if (sort_by == "abs_diff") {
    if ("mean_diff" %in% names(full)) {
      full <- full %>%
        dplyr::arrange(dplyr::desc(abs(.data$mean_diff)))
    } else if ("diff_outcome" %in% names(full)) {
      full <- full %>%
        dplyr::arrange(dplyr::desc(abs(.data$diff_outcome)))
    } else if ("diff_slope" %in% names(full)) {
      full <- full %>%
        dplyr::arrange(dplyr::desc(abs(.data$diff_slope)))
    }
  }

  # --- prepare display p columns -------------------------------------------

  if ("pretty_p_outcome" %in% names(full)) {
    full$p_out_display <- full$pretty_p_outcome
  } else if ("p_outcome" %in% names(full)) {
    full$p_out_display <- full$p_outcome
  }

  has_slope <- !is.null(comps_slope) && nrow(comps_slope) > 0L

  if (has_slope) {
    if ("pretty_p_slope" %in% names(full)) {
      full$p_slope_display <- full$pretty_p_slope
    } else if ("p_slope" %in% names(full)) {
      full$p_slope_display <- full$p_slope
    }
  }

  # --- construct simple display tables ------------------------------------

  y1_name <- object$settings$y1 %||% "y1"
  y2_name <- object$settings$y2 %||% "y2"

  order_desc <- switch(
    sort_by,
    "p"        = "ordered by p-value",
    "abs_diff" = "ordered by absolute difference",
    "none"     = "unsorted"
  )

  # Outcomes: baseGroup TrajType n prop, M_obs, SD_obs, M_diff, SD_diff, t, p, df
  outcome_display <- full %>%
    dplyr::select(
      dplyr::any_of(c(
        "baseGroup", "trajType",
        "n", "prop",
        "mean_obs", "sd_obs",
        "mean_diff", "sd_diff",
        "t_outcome", "p_out_display", "df_outcome"
      ))
    ) %>%
    dplyr::rename(
      TrajType = trajType,
      M_obs    = mean_obs,
      SD_obs   = sd_obs,
      M_diff   = mean_diff,
      SD_diff  = sd_diff,
      t        = t_outcome,
      p        = p_out_display,
      df       = df_outcome
    )

  # Slopes: same parameters as outcomes table, but using slope t/p/df
  if (has_slope &&
      all(c("t_slope", "p_slope_display", "df_slope") %in% names(full))) {

    slopes_display <- full %>%
      dplyr::select(
        dplyr::any_of(c(
          "baseGroup", "trajType",
          "n", "prop",
          "mean_obs", "sd_obs",
          "mean_diff", "sd_diff",
          "t_slope", "p_slope_display", "df_slope"
        ))
      ) %>%
      dplyr::rename(
        TrajType = trajType,
        M_obs    = mean_obs,
        SD_obs   = sd_obs,
        M_diff   = mean_diff,
        SD_diff  = sd_diff,
        t        = t_slope,
        p        = p_slope_display,
        df       = df_slope
      )
  } else {
    slopes_display <- NULL
  }

  # --- print nicely --------------------------------------------------------
  cat("<Summary of SRTM Analysis>\n\n")

  # Outcomes block
  if (nrow(outcome_display) > 0L) {
    cat(glue::glue(
      "Comparing outcomes at {y2_name} ({order_desc}):\n"
    ))
    print(outcome_display)
  } else {
    cat("No outcome comparisons available.\n")
  }

  # Slopes block
  if (!is.null(slopes_display) && nrow(slopes_display) > 0L) {
    cat("\n")
    cat(glue::glue(
      "Comparing slopes between {y1_name} and {y2_name} ({order_desc}):\n"
    ))
    print(slopes_display)
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

