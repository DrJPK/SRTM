#' Plot saved cut-points from an SRTM analysis object
#'
#' @description
#' Convenience wrapper for [plotCutPoints()] that works on an
#' `"srtm_analysis"` object returned by [SRTMAnalyse()]. It looks up any
#' stored `srtm_group_suggestion` objects (from calls to [findGroups()]
#' inside `SRTMAnalyse()`), allows the user to choose one via a simple
#' menu (if `which` is not supplied), and then produces the corresponding
#' density + cut-point plot.
#'
#' @param x An object of class `"srtm_analysis"`, typically the output of
#'   [SRTMAnalyse()].
#' @param which Optional character or integer identifying which stored
#'   group suggestion to plot. If `NULL` (default), a console menu is
#'   shown listing all available suggestions and the user is prompted to
#'   choose one.
#' @param ... Additional arguments passed on to [plotCutPoints()], such
#'   as `x_label`, `annotate`, `decimals`, `shade`, or `palette`.
#'
#' @details
#' `SRTMAnalyse()` stores all internal [findGroups()] results in the
#' `group_params` component of its output, as a list with elements:
#'
#' \itemize{
#'   \item `baseline_overall` — overall baseline grouping on `y1`
#'         (if used).
#'   \item `traj_overall` — overall trajectory grouping on `m01`
#'         (if used).
#'   \item `traj_by_base` — a named list of `srtm_group_suggestion`
#'         objects for each `baseGroup` level (grouping on `m01`).
#'   \item `baseline_by_traj` — a named list of `srtm_group_suggestion`
#'         objects for each `trajGroup` level (grouping on `y1`).
#' }
#'
#' This helper flattens these into a single named vector of choices, such
#' as:
#'
#' \itemize{
#'   \item `"Baseline (overall)"`,
#'   \item `"Trajectory (overall)"`,
#'   \item `"Trajectory within baseline A"`,
#'   \item `"Trajectory within baseline B"`,
#'   \item `"Baseline within trajectory ↟"`, etc.
#' }
#'
#' If `which` is:
#' \itemize{
#'   \item `NULL` — a menu of these labels is shown using
#'         [utils::menu()], and the user’s choice is used.
#'   \item a character string — it is matched against the available
#'         labels (exact match).
#'   \item an integer — it is treated as a 1-based index into the list
#'         of available labels.
#' }
#'
#' The selected `srtm_group_suggestion` object is then passed directly to
#' [plotCutPoints()], with any additional `...` arguments forwarded.
#'
#' @return
#' A `ggplot` object produced by [plotCutPoints()]. The plot is not
#' printed automatically, so it can be composed with other plots (e.g.
#' via patchwork).
#'
#' @examples
#' \dontrun{
#' x <- SRTMAnalyse(SRTM_synth_data, group_first = "baseline")
#'
#' # interactive choice via menu
#' p <- plotSRTMCutPoints(x)
#' p
#'
#' # or explicitly:
#' p2 <- plotSRTMCutPoints(x, which = "Baseline (overall)")
#' p3 <- plotSRTMCutPoints(x, which = 2)  # second item in the menu
#' }
#'
#' @export
plotSRTMCutPoints <- function(x, which = NULL, ...) {
  if (!inherits(x, "srtm_analysis")) {
    rlang::abort(
      "`x` must be an 'srtm_analysis' object (output of SRTMAnalyse()).",
      class = "srtm_plotSRTMCutPoints_bad_x"
    )
  }

  gp <- x$group_params
  if (is.null(gp) || !is.list(gp)) {
    rlang::abort(
      "No `group_params` component found in `x`. Has SRTMAnalyse() been updated?",
      class = "srtm_plotSRTMCutPoints_no_gp"
    )
  }

  available <- list()
  labels    <- character(0)
  x_labels  <- character(0)

  # 1) overall baseline
  if (!is.null(gp$baseline_overall)) {
    available[["Baseline (overall)"]] <- gp$baseline_overall
    labels <- c(labels, "Baseline (overall)")
    x_labels <- c(x_labels,"Baseline Score at `y1`")
  }

  # 2) overall trajectory
  if (!is.null(gp$traj_overall)) {
    available[["Trajectory (overall)"]] <- gp$traj_overall
    labels <- c(labels, "Trajectory (overall)")
    x_labels <- c(x_labels,"Trajectory Slope at `y1`")
  }

  # 3) trajectory within each baseline group
  if (length(gp$traj_by_base) > 0L) {
    for (nm in names(gp$traj_by_base)) {
      gp_i <- gp$traj_by_base[[nm]]
      if (!inherits(gp_i, "srtm_group_suggestion")) next
      lab <- glue::glue("Trajectory within baseline {nm}")
      available[[lab]] <- gp_i
      labels <- c(labels, lab)
      x_labels <- c(x_labels,"Trajectory Slope at `y1`")
    }
  }

  # 4) baseline within each trajectory group
  if (length(gp$baseline_by_traj) > 0L) {
    for (nm in names(gp$baseline_by_traj)) {
      gp_i <- gp$baseline_by_traj[[nm]]
      if (!inherits(gp_i, "srtm_group_suggestion")) next
      lab <- glue::glue("Baseline within trajectory {nm}")
      available[[lab]] <- gp_i
      labels <- c(labels, lab)
      x_labels <- c(x_labels,"Baseline Score at `y1`")
    }
  }

  if (length(available) == 0L) {
    rlang::abort(
      "No stored group suggestions found in `x$group_params`.",
      class = "srtm_plotSRTMCutPoints_empty"
    )
  }

  # decide which one to use
  if (is.null(which)) {
    idx <- utils::menu(
      choices = labels,
      title   = "Select a set of cut-points to plot:"
    )
    if (idx == 0L) {
      rlang::abort(
        "No selection made.",
        class = "srtm_plotSRTMCutPoints_no_selection"
      )
    }
    sel_label <- labels[[idx]]
    x_label <- x_labels[[idx]]
  } else if (is.numeric(which) && length(which) == 1L) {
    idx <- as.integer(which)
    if (idx < 1L || idx > length(labels)) {
      rlang::abort(
        glue::glue("`which` index {idx} is out of range (1..{length(labels)})."),
        class = "srtm_plotSRTMCutPoints_bad_index"
      )
    }
    sel_label <- labels[[idx]]
    x_label <- x_labels[[idx]]
  } else if (is.character(which) && length(which) == 1L) {
    if (!which %in% labels) {
      rlang::abort(
        glue::glue(
          "`which` = '{which}' not found among available labels:\n",
          "{paste(labels, collapse = ', ')}"
        ),
        class = "srtm_plotSRTMCutPoints_bad_label"
      )
    }
    sel_label <- which
    x_label <- "score"
  } else {
    rlang::abort(
      "`which` must be NULL, a single integer index, or a single character label.",
      class = "srtm_plotSRTMCutPoints_bad_which"
    )
  }

  gp_sel <- available[[sel_label]]

  # delegate actual plotting to plotCutPoints()
  plotCutPoints(gp_sel,
                title_var = sel_label,
                x_label = x_label,
                ...)
}
