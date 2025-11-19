#' Resolve canonical grouping variable names for an SRTM analysis
#'
#' @description
#' Internal helper that standardises which columns to use for baseline and
#' trajectory grouping when working with an \code{"srtm_analysis"} object.
#' It prefers semantic trajectory labels (\code{trajType}) when available
#' and falls back to \code{trajGroup} with a warning. It also returns
#' primary vs secondary grouping variables based on \code{group_first}.
#'
#' @param results An object of class \code{"srtm_analysis"}.
#' @param debug Logical; if \code{TRUE}, emits informative messages via
#'   \code{rlang::inform()}.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{base_col} — name of the baseline group column (usually \code{"baseGroup"}).
#'     \item \code{traj_col} — name of the trajectory group column (preferably \code{"trajType"}).
#'     \item \code{primary_group_var} — grouping variable used first, depending on \code{group_first}.
#'     \item \code{secondary_group_var} — grouping variable used second.
#'     \item \code{group_first} — the resolved grouping order (\code{"baseline"} or \code{"slope"}).
#'   }
#'
#' @keywords internal
srtm_resolve_group_vars <- function(results, debug = FALSE) {

  if (!inherits(results, "srtm_analysis")) {
    rlang::abort(
      "`results` must be an object of class 'srtm_analysis'.",
      class = "srtm_groupvars_bad_results"
    )
  }

  df       <- results$data
  settings <- results$settings %||% list()

  # Resolve grouping order
  group_first <- settings$group_first %||% "baseline"
  group_first <- match.arg(group_first, c("baseline", "slope"))

  # Baseline group column
  if ("baseGroup" %in% names(df)) {
    base_col <- "baseGroup"
  } else {
    rlang::abort(
      "No `baseGroup` column found in `results$data`.",
      class = "srtm_groupvars_no_base"
    )
  }

  # Trajectory grouping column: prefer trajType, then fall back to trajGroup
  if ("trajType" %in% names(df)) {
    traj_col <- "trajType"
  } else if ("trajGroup" %in% names(df)) {
    traj_col <- "trajGroup"
    rlang::warn(
      paste0(
        "Using `trajGroup` for trajectory grouping because `trajType` is ",
        "not present in `results$data`. Consider updating the analysis to ",
        "compute `trajType` for semantic trajectory labels."
      ),
      class = "srtm_groupvars_traj_fallback"
    )
  } else {
    rlang::abort(
      "No `trajType` or `trajGroup` column found in `results$data`.",
      class = "srtm_groupvars_no_traj"
    )
  }

  if (debug) {
    rlang::inform(
      glue::glue(
        "srtm_resolve_group_vars(): group_first = '{group_first}', ",
        "base_col = '{base_col}', traj_col = '{traj_col}'."
      ),
      class = "srtm_groupvars_debug"
    )
  }

  # Decide primary vs secondary group variable
  if (identical(group_first, "baseline")) {
    primary_group_var   <- base_col
    secondary_group_var <- traj_col
  } else { # slope first
    primary_group_var   <- traj_col
    secondary_group_var <- base_col
  }

  list(
    base_col            = base_col,
    traj_col            = traj_col,
    primary_group_var   = primary_group_var,
    secondary_group_var = secondary_group_var,
    group_first         = group_first
  )
}
