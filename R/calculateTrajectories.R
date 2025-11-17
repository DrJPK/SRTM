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
