# Internal state for time handling
.srtm_time_state <- new.env(parent = emptyenv())

srtm_reset_time_state <- function() {
  rm(list = ls(.srtm_time_state), envir = .srtm_time_state)
  .srtm_time_state$mode        <- NULL   # "duration" or "dates"
  .srtm_time_state$unit_label  <- NULL   # e.g. "days", "weeks"
  .srtm_time_state$dt01        <- NULL
  .srtm_time_state$dt12        <- NULL
  .srtm_time_state$t0          <- NULL   # numeric or Date
  .srtm_time_state$t1          <- NULL
  .srtm_time_state$t2          <- NULL
  .srtm_time_state$return_idx  <- 0L     # for dates mode
}

srtm_prompt_time_unit <- function() {
  rlang::inform(
    "Please choose a time unit (used for labels and axis titles):"
  )
  choice <- utils::menu(
    choices = c(
      "Time units",
      "Days",
      "Weeks",
      "Months",
      "Quarters",
      "Years",
      "Other (type your own)"
    ),
    title = "Select time unit"
  )

  if (choice == 0L) {
    return("Time units")
  }

  unit_map <- c(
    "Time units",
    "days",
    "weeks",
    "months",
    "quarters",
    "years"
  )

  if (choice %in% 1:6) {
    unit_label <- unit_map[choice]
  } else {
    txt <- readline("Enter a custom unit name (e.g., 'school terms'): ")
    unit_label <- if (nzchar(txt)) txt else "Time units"
  }

  unit_label
}

#' Internal helper to obtain a valid time period
#'
#' This helper can work in two modes:
#' * *Duration* mode: the user enters time differences directly (e.g. "1" week)
#'   and a time-unit label is stored for later use.
#' * *Dates* mode: the user enters three dates (Historical, Pre, Post) once;
#'   the differences in days between Historical–Pre (dt01) and Pre–Post (dt12)
#'   are then reused on subsequent calls.
#'
#' The chosen unit label and/or dates are stored in an internal environment
#' (`.srtm_time_state`) so they can be used later (e.g. in plotting and
#' in the SRTMAnalyse settings).
#'
#' @keywords internal
getTimePeriod <- function(time_period = NULL,
                          interactive = TRUE,
                          label = "between two time points") {

  # If user has supplied a numeric time_period, just validate and return ------
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

  # If not given and not interactive, abort ----------------------------------
  if (!interactive) {
    rlang::abort(
      "`time_period` must be supplied when `interactive = FALSE`.",
      class = "srtm_calcTrajectories_missing_time"
    )
  }

  # If we've already chosen "dates" mode, just reuse stored dt01/dt12 --------
  if (identical(.srtm_time_state$mode, "dates")) {
    idx <- .srtm_time_state$return_idx %||% 0L

    if (is.null(.srtm_time_state$dt01) || is.null(.srtm_time_state$dt12)) {
      rlang::abort(
        "Internal time-state for dates mode is incomplete.",
        class = "srtm_calcTrajectories_bad_date_state"
      )
    }

    if (idx == 0L) {
      .srtm_time_state$return_idx <- 1L
      return(.srtm_time_state$dt01)
    } else {
      .srtm_time_state$return_idx <- idx + 1L
      return(.srtm_time_state$dt12)
    }
  }

  # If we already chose duration mode and no explicit time_period is given,
  # assume this is the *second* (or later) call and just prompt for the
  # numeric value in the same units.
  if (identical(.srtm_time_state$mode, "duration")) {
    unit_label <- .srtm_time_state$unit_label %||% "Time units"
    prompt <- glue::glue(
      "Previously you entered a time unit of '{unit_label}'.\n",
      "Enter the time period for {label} in the same unit: "
    )
    ans <- readline(prompt)
    tp  <- suppressWarnings(as.numeric(ans))

    if (is.na(tp) || tp <= 0) {
      rlang::abort(
        "`time_period` must be a single positive numeric value.",
        class = "srtm_calcTrajectories_bad_time_period"
      )
    }

    return(tp)
  }

  # --------------------------------------------------------------------------
  # No mode chosen yet: ask duration vs dates
  # --------------------------------------------------------------------------
  rlang::inform(
    glue::glue(
      "No `time_period` supplied for {label}. You can either:\n",
      "  1) Enter time period(s) directly, or\n",
      "  2) Enter three dates (Historical, Pre, Post) and use their differences."
    )
  )

  choice <- utils::menu(
    c("Enter time period directly", "Enter three dates (Historical, Pre, Post)"),
    title = "Choose how to specify time:"
  )

  if (choice == 0L) {
    rlang::abort(
      "No choice made for time period specification.",
      class = "srtm_calcTrajectories_no_time_choice"
    )
  }

  # --------------------------------------------------------------------------
  # Option 1: duration mode (direct entry, with unit)
  # --------------------------------------------------------------------------
  if (choice == 1L) {
    ans <- readline("Enter the time period (e.g., number of days/weeks): ")
    tp  <- suppressWarnings(as.numeric(ans))

    if (is.na(tp) || tp <= 0) {
      rlang::abort(
        "`time_period` must be a single positive numeric value.",
        class = "srtm_calcTrajectories_bad_time_period"
      )
    }

    unit_label <- srtm_prompt_time_unit()

    .srtm_time_state$mode       <- "duration"
    .srtm_time_state$unit_label <- unit_label
    # dt01 / dt12 and t0/t1/t2 will be implied later by SRTMAnalyse,
    # using dt01, dt12 and the convention t1 = 0, t0 = -dt01, t2 = dt12.

    rlang::inform(
      glue::glue(
        "Using time_period = {tp} ({unit_label}) for {label}."
      )
    )

    return(tp)
  }

  # --------------------------------------------------------------------------
  # Option 2: dates mode: ask for full Historical / Pre / Post once
  # --------------------------------------------------------------------------
  rlang::inform(
    "Please enter three dates in ISO format (YYYY-MM-DD)."
  )

  hist_str <- readline("Historical (y0) date (e.g. 2025-02-01): ")
  pre_str  <- readline("Pre (y1) date        (e.g. 2025-03-01): ")
  post_str <- readline("Post (y2) date       (e.g. 2025-06-01): ")

  hist_date <- suppressWarnings(as.Date(hist_str))
  pre_date  <- suppressWarnings(as.Date(pre_str))
  post_date <- suppressWarnings(as.Date(post_str))

  if (any(is.na(c(hist_date, pre_date, post_date)))) {
    rlang::abort(
      "Could not parse one or more dates. Please use ISO format (YYYY-MM-DD).",
      class = "srtm_calcTrajectories_bad_dates"
    )
  }

  if (!(hist_date < pre_date && pre_date < post_date)) {
    rlang::abort(
      "Dates must satisfy Historical < Pre < Post.",
      class = "srtm_calcTrajectories_bad_date_order"
    )
  }

  dt01 <- as.numeric(pre_date  - hist_date)
  dt12 <- as.numeric(post_date - pre_date)

  if (dt01 <= 0 || dt12 <= 0) {
    rlang::abort(
      "Non-positive time differences derived from dates.",
      class = "srtm_calcTrajectories_non_positive_diff"
    )
  }

  rlang::inform(
    glue::glue(
      "Using dates:\n",
      "  Historical (y0): {hist_date}\n",
      "  Pre        (y1): {pre_date}  (dt01 = {dt01} days)\n",
      "  Post       (y2): {post_date} (dt12 = {dt12} days)\n"
    )
  )

  .srtm_time_state$mode       <- "dates"
  .srtm_time_state$unit_label <- "days"
  .srtm_time_state$dt01       <- dt01
  .srtm_time_state$dt12       <- dt12
  .srtm_time_state$t0         <- hist_date
  .srtm_time_state$t1         <- pre_date
  .srtm_time_state$t2         <- post_date
  .srtm_time_state$return_idx <- 1L  # first call returns dt01

  dt01
}
