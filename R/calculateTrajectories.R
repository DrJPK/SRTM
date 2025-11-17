#' Calculate individual trajectory slopes between two time points
#'
#' @description
#' Computes the trajectory slope between two numeric variables (for example
#' `y0` and `y1`) for each individual in a dataset, given the time elapsed
#' between those measurements. The result is a numeric vector that can be
#' attached to the original data (e.g., as `m01`).
#'
#' @param data A data frame or tibble containing the variables specified by
#'   `y0` and `y1`.
#' @param y0 Column specification for the earlier measurement (e.g.,
#'   historical or baseline score). Can be a column name as a string (such
#'   as `"y0"`) or a bare column name (such as `y0`). Defaults to `"y0"`.
#' @param y1 Column specification for the later measurement (e.g., baseline or
#'   post score). Can be a column name as a string or a bare column name.
#'   Defaults to `"y1"`.
#' @param time_period Numeric scalar giving the elapsed time between `y0` and
#'   `y1` (e.g., number of days, weeks, or terms). If missing and
#'   `interactive = TRUE`, the user is prompted to either enter a time period
#'   directly or supply two dates from which a difference in days is
#'   computed. If missing and `interactive = FALSE`, an error is raised.
#' @param interactive Logical. If `TRUE` (default), interactive prompts are
#'   used when `time_period` is not supplied. If `FALSE`, `time_period` must
#'   be provided explicitly.
#'
#' @details
#' The function verifies that `data` is a data frame, that `y0` and `y1`
#' exist and are numeric, and that the resolved `time_period` is a single
#' positive numeric value. When `time_period` is not supplied and
#' `interactive = TRUE`, the user can:
#'
#' \itemize{
#'   \item enter a time period directly (e.g., `"90"` days), or
#'   \item enter two ISO-format dates (YYYY-MM-DD), in which case the time
#'         period is computed as the difference in days.
#' }
#'
#' The trajectory slope for each row is then computed as
#' \deqn{
#'   m_{01} = \frac{y_1 - y_0}{\text{time\_period}}.
#' }
#'
#' No imputation is performed; if `y0` or `y1` contain `NA`, the corresponding
#' slope will also be `NA`.
#'
#' @return
#' A numeric vector of length `nrow(data)` containing the slopes between `y0`
#' and `y1`, using the supplied (or interactively determined) `time_period`.
#'
#' @examples
#' # Using synthetic data from SRTM
#' df <- generateSynthData(n = 10, seed = 123)
#'
#' # Suppose y0 and y1 are 90 days apart
#' df$m01 <- calculateTrajectories(
#'   data        = df,
#'   y0          = "y0",
#'   y1          = "y1",
#'   time_period = 90,
#'   interactive = FALSE
#' )
#'
#' @export

calculateTrajectories <- function(data,
                                  y0 = "y0",
                                  y1 = "y1",
                                  time_period,
                                  interactive = TRUE) {

  # --- basic checks --------------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_calcTrajectories_bad_data"
    )
  }

  if (is.character(y0) && length(y0) == 1L) {
    y0_name <- y0
  } else {
    y0_sym  <- rlang::ensym(y0)
    y0_name <- rlang::as_string(y0_sym)
  }

  if (is.character(y1) && length(y1) == 1L) {
    y1_name <- y1
  } else {
    y1_sym  <- rlang::ensym(y1)
    y1_name <- rlang::as_string(y1_sym)
  }

  #rlang::inform(glue::glue("DEBUG: calculateTrajectories using y0 = '{y0_name}', y1 = '{y1_name}'"))

  if (!y0_name %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{y0_name}` not found in `data`."),
      class = "srtm_calcTrajectories_missing_col"
    )
  }

  if (!y1_name %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{y1_name}` not found in `data`."),
      class = "srtm_calcTrajectories_missing_col"
    )
  }

  x0 <- data[[y0_name]]
  x1 <- data[[y1_name]]

  if (!is.numeric(x0)) {
    rlang::abort(
      glue::glue("Column `{y0_name}` must be numeric."),
      class = "srtm_calcTrajectories_non_numeric"
    )
  }

  if (!is.numeric(x1)) {
    rlang::abort(
      glue::glue("Column `{y1_name}` must be numeric."),
      class = "srtm_calcTrajectories_non_numeric"
    )
  }

  # --- obtain time_period via helper ---------------------------------------
  tp <- getTimePeriod(
    time_period = if (missing(time_period)) NULL else time_period,
    interactive = interactive,
    label       = glue::glue("{y0_name} and {y1_name}")
  )

  # --- compute slopes ------------------------------------------------------
  slopes <- (x1 - x0) / tp

  slopes
}
