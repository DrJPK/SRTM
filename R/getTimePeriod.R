#' Reset internal SRMT time-state
#'
#' Resets the internal environment used to store time-handling state for SRMT
#' analyses. This clears any previously selected mode (duration vs dates),
#' stored unit labels, date differences and cached dates used by
#' \code{\link{getTimePeriod}()} and higher-level functions such as
#' \code{SRTMAnalyse()}.
#'
#' This function is intended for internal package use only, for example before
#' starting a new SRMT analysis session or when re-running interactive
#' workflows to ensure that no stale time information is reused.
#'
#' @return Invisibly resets the contents of \code{.srtm_time_state}. Called for
#'   its side effects.
#'
#' @keywords internal
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


#' Interactive helper to choose a time-unit label
#'
#' Presents a simple menu in the console to select a time-unit label for use
#' in SRMT outputs (axis titles, summaries, etc.). The user can choose from a
#' small set of common units (days, weeks, months, quarters, years) or type a
#' custom unit label (e.g., "school terms").
#'
#' This helper is called by \code{\link{getTimePeriod}()} when working in
#' "duration" mode. It does not modify the internal time state directly; it
#' only returns the chosen label.
#'
#' @return A character scalar giving the chosen unit label (e.g. \code{"days"},
#'   \code{"weeks"}, \code{"school terms"}). If the user cancels or enters an
#'   empty custom label, defaults to \code{"Time units"}.
#'
#' @keywords internal
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
#' `getTimePeriod()` obtains a positive time difference to be used in SRMT
#' analyses, either directly from a supplied numeric value or interactively
#' from the user. It can operate in two conceptual modes:
#'
#' \itemize{
#'   \item \emph{Duration mode}: the user enters time differences directly
#'     (e.g., "1" week) and a time-unit label is stored for later use in
#'     labelling outputs.
#'   \item \emph{Dates mode}: the user enters three dates once (Historical y0,
#'     Pre y1, Post y2). The differences in days between Historical–Pre
#'     (\code{dt01}) and Pre–Post (\code{dt12}) are then reused on subsequent
#'     calls.
#' }
#'
#' The chosen mode, unit label and/or date-derived differences are cached in an
#' internal environment \code{.srtm_time_state} so they can be reused by
#' \code{SRTMAnalyse()} and related helpers without repeatedly asking the user
#' for the same information.
#'
#' @param time_period Optional numeric scalar giving the time difference
#'   directly. If supplied, it must be a single positive finite number. In this
#'   case, `getTimePeriod()` simply validates and returns this value without
#'   modifying internal state.
#'
#' @param interactive Logical. If \code{TRUE} (default), the function may
#'   prompt the user via the console to supply timing information and/or choose
#'   between duration and dates modes. If \code{FALSE}, and
#'   \code{time_period} is \code{NULL}, the function aborts with an error
#'   rather than entering interactive prompts.
#'
#' @param label Character description of the interval being requested, used in
#'   interactive prompts (e.g., \code{"between baseline and follow-up"}). This
#'   does not affect the returned value but helps make prompts clearer.
#'
#' @return A single positive numeric value representing the time difference:
#'   \itemize{
#'     \item If \code{time_period} is supplied: the validated value of
#'       \code{time_period}.
#'     \item In duration mode: the numeric value entered by the user, in the
#'       units implied by the chosen time-unit label.
#'     \item In dates mode: on the first call, the difference
#'       \code{dt01 = y1 - y0} in days; on the second call, the difference
#'       \code{dt12 = y2 - y1} in days; on subsequent calls, \code{dt12} is
#'       reused unless the state is reset.
#'   }
#'
#' @details
#' The function maintains a simple internal state machine via the
#' \code{.srtm_time_state} environment:
#'
#' \itemize{
#'   \item When no mode has been chosen and \code{time_period} is
#'     \code{NULL}, the user is asked whether to:
#'     \enumerate{
#'       \item enter time periods directly (duration mode), or
#'       \item enter three dates (dates mode).
#'     }
#'
#'   \item In \strong{duration mode}, the first call both records the numeric
#'     value (for the current interval) and prompts for a time-unit label via
#'     \code{\link{srtm_prompt_time_unit}()}. Subsequent calls (with
#'     \code{time_period = NULL}) reuse the chosen label in prompts and
#'     simply ask for another numeric time period in the same units.
#'
#'   \item In \strong{dates mode}, the user is asked once to enter three dates
#'     in ISO format (YYYY-MM-DD) corresponding to Historical (y0), Pre (y1)
#'     and Post (y2). These are validated to ensure
#'     \code{Historical < Pre < Post}, and the differences in days
#'     (\code{dt01}, \code{dt12}) are stored. The first call returns
#'     \code{dt01}, the second and subsequent calls return \code{dt12}.
#' }
#'
#' Higher-level SRMT functions use these returned values to construct a
#' time-scale with \code{t1 = 0}, \code{t0 = -dt01} and \code{t2 = dt12}. The
#' supplementary information in \code{.srtm_time_state} (e.g. unit labels and
#' original dates) can also be used for labelling and reporting in plots and
#' summaries.
#'
#' To clear all stored timing information and start afresh (for a new analysis
#' or session), call \code{\link{srtm_reset_time_state}()}.
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
