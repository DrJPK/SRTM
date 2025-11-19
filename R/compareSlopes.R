#' Compare observed and expected slopes within SRMT trajectory types
#'
#' `compareSlopes()` computes paired comparisons between an observed slope
#' (e.g., the post-event slope \code{m12}) and an expected slope
#' (e.g., the pre-event or model-expected slope \code{m01}) within
#' combinations of baseline and trajectory types. For each
#' \code{baseGroups} × \code{trajGroups} combination, it performs a
#' one-sample t-test on the difference \code{obsSlope - expSlope}, testing
#' whether the mean difference is equal to zero.
#'
#' @param data A data frame or tibble containing the observed slope,
#'   expected slope, and grouping variables. Must include the columns
#'   referenced by \code{obsSlope}, \code{expSlope}, \code{baseGroups}, and
#'   \code{trajGroups}.
#'
#' @param obsSlope The observed slope variable (default \code{m12}).
#'
#' @param expSlope The expected slope variable (default \code{m01}).
#'
#' @param baseGroups Baseline grouping variable (default \code{baseGroup}).
#'
#' @param trajGroups Trajectory \emph{type} grouping variable
#'   (default \code{trajType}).
#'
#' @return A tibble with one row per \code{baseGroups} × \code{trajGroups}
#'   combination, containing:
#'   \itemize{
#'     \item the grouping variables (e.g., \code{baseGroup}, \code{trajType}),
#'     \item \code{t_value}: t statistic for \code{obsSlope - expSlope},
#'     \item \code{diff}: mean difference \code{mean(obsSlope - expSlope)},
#'     \item \code{p}: raw p-value,
#'     \item \code{pretty_p}: formatted p-value,
#'     \item \code{df}: degrees of freedom.
#'   }
#'
#' @section Interpretation:
#' A positive \code{diff} indicates that, on average, the observed post-event
#' slope is steeper than expected; a negative value indicates a flatter or
#' declining slope relative to expectations.
#'
#' @export
compareSlopes <- function(data,
                          obsSlope   = m12,
                          expSlope   = m01,
                          baseGroups = baseGroup,
                          trajGroups = trajType) {

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_compare_slopes_bad_data"
    )
  }

  obs_name  <- resolve_name(rlang::enquo(obsSlope),  arg = "obsSlope")
  exp_name  <- resolve_name(rlang::enquo(expSlope),  arg = "expSlope")
  base_name <- resolve_name(rlang::enquo(baseGroups), arg = "baseGroups")
  traj_name <- resolve_name(rlang::enquo(trajGroups), arg = "trajGroups")

  required_cols <- c(obs_name, exp_name, base_name, traj_name)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue(
        "Missing required columns: {paste(missing_cols, collapse = ', ')}."
      ),
      class = "srtm_compare_slopes_missing_cols"
    )
  }

  if (!is.numeric(data[[obs_name]])) {
    rlang::abort(
      glue::glue("Column `{obs_name}` must be numeric."),
      class = "srtm_compare_slopes_non_numeric"
    )
  }

  if (!is.numeric(data[[exp_name]])) {
    rlang::abort(
      glue::glue("Column `{exp_name}` must be numeric."),
      class = "srtm_compare_slopes_non_numeric"
    )
  }

  df <- tibble::as_tibble(data)

  if (!is.factor(df[[base_name]])) {
    df[[base_name]] <- factor(df[[base_name]])
  }
  if (!is.factor(df[[traj_name]])) {
    df[[traj_name]] <- factor(df[[traj_name]])
  }

  .srtm_compare_diffs(
    df        = df,
    obs_name  = obs_name,
    exp_name  = exp_name,
    base_name = base_name,
    traj_name = traj_name
  )
}
