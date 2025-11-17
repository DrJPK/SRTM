#' Internal helper to obtain a valid time period
#'
#' @keywords internal
getTimePeriod <- function(time_period = NULL,
                          interactive = TRUE,
                          label = "between two time points") {

  # If given, just validate and return
  if (!is.null(time_period)) {
    if (!is.numeric(time_period) ||
        length(time_period) != 1L ||
        !is.finite(time_period) ||
        time_period <= 0) {
      rlang::abort(
        "`time_period` must be a single positive numeric value.",
        class = "srtm_calcTrajectories_bad_time_period"
      )
    }
    return(time_period)
  }

  # If not given and not interactive, we have to abort
  if (!interactive) {
    rlang::abort(
      "`time_period` must be supplied when `interactive = FALSE`.",
      class = "srtm_calcTrajectories_missing_time"
    )
  }

  # Interactive prompt
  rlang::inform(
    glue::glue(
      "No `time_period` supplied for {label}. You can either:\n",
      "  1) Enter a time period directly, or\n",
      "  2) Enter two dates and have the difference calculated."
    )
  )

  choice <- utils::menu(
    c("Enter time period directly", "Enter two dates"),
    title = "Choose how to specify time:"
  )

  if (choice == 0L) {
    rlang::abort(
      "No choice made for time period specification.",
      class = "srtm_calcTrajectories_no_time_choice"
    )
  }

  if (choice == 1L) {
    ans <- readline("Enter the time period (e.g., number of days): ")
    tp  <- suppressWarnings(as.numeric(ans))

    if (is.na(tp) || tp <= 0) {
      rlang::abort(
        "`time_period` must be a single positive numeric value.",
        class = "srtm_calcTrajectories_bad_time_period"
      )
    }

    return(tp)
  }

  # choice == 2L: ask for two dates
  start_str <- readline(
    glue::glue("Enter the start date for {label} (e.g. 2025-02-01): ")
  )
  end_str   <- readline(
    glue::glue("Enter the end date for {label} (e.g. 2025-03-01): ")
  )

  start_date <- suppressWarnings(as.Date(start_str))
  end_date   <- suppressWarnings(as.Date(end_str))

  if (is.na(start_date) || is.na(end_date)) {
    rlang::abort(
      "Could not parse one or both dates. Please use ISO format (YYYY-MM-DD).",
      class = "srtm_calcTrajectories_bad_dates"
    )
  }

  diff_days <- as.numeric(end_date - start_date)

  if (diff_days <= 0) {
    rlang::abort(
      "The end date must be after the start date.",
      class = "srtm_calcTrajectories_non_positive_diff"
    )
  }

  rlang::inform(
    glue::glue("Using time_period = {diff_days} (difference in days).")
  )

  diff_days
}
