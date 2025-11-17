#' Summarise observed and expected outcomes by Base × Trajectory group
#'
#' @description
#' Given an SRTM dataset containing baseline and trajectory groups plus
#' observed and expected outcomes (e.g. `y2` and `exp_y2`), this helper
#' returns a summary table for each Base × Traj combination:
#' \itemize{
#'   \item number of cases and proportion,
#'   \item mean observed and expected outcome,
#'   \item mean difference (obs - exp),
#'   \item SD and SE of the difference.
#' }
#'
#' @param data A data frame or tibble.
#' @param obs Column specifying the observed outcome (bare or string).
#' @param exp Column specifying the expected outcome (bare or string).
#' @param base_group Column specifying baseline groups (bare or string).
#' @param traj_group Column specifying trajectory groups (bare or string).
#' @param drop Logical. If `TRUE` (default), rows with missing groups or
#'   missing obs/exp are dropped before summarising.
#'
#' @return
#' A tibble with columns:
#' \itemize{
#'   \item `baseGroup`, `trajGroup`
#'   \item `n`, `proportion`
#'   \item `mean_obs`, `mean_exp`
#'   \item `mean_diff` (obs - exp)
#'   \item `sd_diff`, `se_diff`
#' }
#'
#' @examples
#' x <- SRTMAnalyse(SRTM_synth_data)
#' summariseGroupOutcomes(
#'   x$data,
#'   obs        = y2,
#'   exp        = exp_y2,
#'   base_group = baseGroup,
#'   traj_group = trajGroup
#' )
#'
#' @export
summariseGroupOutcomes <- function(data,
                                   obs,
                                   exp,
                                   base_group = "baseGroup",
                                   traj_group = "trajGroup",
                                   drop = TRUE) {

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.",
                 class = "srtm_summarise_bad_data")
  }

  # resolve names robustly (using your utils-resolve_name)
  obs_name  <- resolve_name(obs)
  exp_name  <- resolve_name(exp)
  bg_name   <- resolve_name(base_group)
  tg_name   <- resolve_name(traj_group)

  needed <- c(obs_name, exp_name, bg_name, tg_name)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue(
        "Missing required columns in `data`: {paste(missing_cols, collapse = ', ')}."
      ),
      class = "srtm_summarise_missing_cols"
    )
  }

  df <- tibble::as_tibble(data)

  if (drop) {
    df <- df %>%
      dplyr::filter(
        !is.na(.data[[bg_name]]),
        !is.na(.data[[tg_name]]),
        !is.na(.data[[obs_name]]),
        !is.na(.data[[exp_name]])
      )
  }

  if (nrow(df) == 0L) {
    return(
      tibble::tibble(
        baseGroup  = character(0),
        trajGroup  = character(0),
        n          = integer(0),
        proportion = numeric(0),
        mean_obs   = numeric(0),
        mean_exp   = numeric(0),
        mean_diff  = numeric(0),
        sd_diff    = numeric(0),
        se_diff    = numeric(0)
      )
    )
  }

  total_n <- nrow(df)

  out <- df %>%
    dplyr::mutate(
      diff = .data[[obs_name]] - .data[[exp_name]]
    ) %>%
    dplyr::group_by(.data[[bg_name]], .data[[tg_name]]) %>%
    dplyr::summarise(
      n          = dplyr::n(),
      proportion = round(n / total_n, 3),
      mean_obs   = mean(.data[[obs_name]], na.rm = TRUE),
      mean_exp   = mean(.data[[exp_name]], na.rm = TRUE),
      mean_diff  = mean(diff, na.rm = TRUE),
      sd_diff    = stats::sd(diff, na.rm = TRUE),
      se_diff    = sd_diff / sqrt(n),
      .groups    = "drop"
    ) %>%
    dplyr::rename(
      baseGroup = !!rlang::sym(bg_name),
      trajGroup = !!rlang::sym(tg_name)
    )

  out
}
