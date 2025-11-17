#' Count observations in each Base × Trajectory group combination
#'
#' @description
#' Given an SRTM dataset containing `baseGroup` and `trajGroup`, this helper
#' returns a summary table with the number of cases in each group combination
#' and the proportion of the total usable rows they represent.
#'
#' @param data A data frame containing grouping variables.
#' @param base_group Column name (bare or string) for the baseline groups.
#' @param traj_group Column name (bare or string) for the trajectory groups.
#' @param drop Logical. If `TRUE` (default), rows with missing group values are
#'   removed before counting.
#'
#' @return
#' A tibble with columns:
#' - `baseGroup`
#' - `trajGroup`
#' - `n`
#' - `proportion`
#'
#' @examples
#' x <- SRTMAnalyse(SRTM_synth_data)
#' countGroupCombinations(x$data)
#'
#' @export
countGroupCombinations <- function(data,
                                   base_group = "baseGroup",
                                   traj_group = "trajGroup",
                                   drop = TRUE) {

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.",
                 class = "srtm_count_bad_data")
  }

  # resolve names robustly
  bg  <- resolve_name(base_group)
  tg  <- resolve_name(traj_group)

  if (!bg %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{bg}` not found in `data`."),
      class = "srtm_count_missing_bg"
    )
  }
  if (!tg %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{tg}` not found in `data`."),
      class = "srtm_count_missing_tg"
    )
  }

  df <- tibble::as_tibble(data)

  if (drop) {
    df <- df %>% dplyr::filter(!is.na(.data[[bg]]),
                               !is.na(.data[[tg]]))
  }

  total_n <- nrow(df)
  if (total_n == 0) {
    return(
      tibble::tibble(
        baseGroup   = character(0),
        trajGroup   = character(0),
        n           = integer(0),
        proportion  = numeric(0)
      )
    )
  }

  out <- df %>%
    dplyr::group_by(.data[[bg]], .data[[tg]]) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(
      proportion = round(n / sum(n), 3)
    ) %>%
    dplyr::rename(
      baseGroup = !!bg,
      trajGroup = !!tg
    )

  out
}
