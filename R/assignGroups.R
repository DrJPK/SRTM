#' Assign group labels based on a group suggestion object
#'
#' @description
#' Assigns group membership to each case in a dataset using the information
#' stored in an `"srtm_group_suggestion"` object produced by [findGroups()].
#' The grouping is based on a single numeric time point (e.g. `y1`), with
#' cut-points derived from the local minima of a kernel density estimate.
#'
#' @param data A data frame or tibble containing the time-point variable named
#'   in `group_params$time_var`.
#' @param group_params An object of class `"srtm_group_suggestion"`, typically
#'   returned by [findGroups()]. It must contain at least:
#'   \itemize{
#'     \item `time_var` — the name of the variable to use for grouping.
#'     \item `nGroups` — the final number of groups to form.
#'     \item `minima_x` — the positions of local minima in the density estimate.
#'   }
#'   If `group_params` is `NULL` or not of the correct class and
#'   `interactive = TRUE`, the function calls [findGroups()] interactively to
#'   construct a suitable object.
#' @param interactive Logical. If `TRUE` (default), and `group_params` is not
#'   supplied or invalid, the user is guided through an interactive process to
#'   select a time variable and determine the number of groups via [findGroups()].
#'   If `FALSE`, `group_params` must be a valid `"srtm_group_suggestion"` object.
#'
#' @details
#' The function uses the local minima stored in `group_params$minima_x` as
#' potential boundaries between groups. If the number of minima exceeds
#' `nGroups - 1`, a subset of minima is selected to provide approximately
#' evenly spaced cut-points across the distribution. These cut-points are then
#' used to partition the chosen time variable into `nGroups` ordered categories,
#' labelled `"A"`, `"B"`, `"C"`, and so on.
#'
#' Cases with missing values on the grouping variable are assigned `NA` for the
#' returned group label. If there are fewer non-missing observations than
#' `nGroups`, or there are insufficient minima to support the requested number
#' of groups, an error is raised.
#'
#' @return
#' A factor vector of length `nrow(data)` containing group labels:
#' `"A"`, `"B"`, `"C"`, … up to the

assignGroups <- function(data,
                         group_params = NULL,
                         interactive  = TRUE) {

  # --- basic checks --------------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_assignGroups_bad_data"
    )
  }

  # if group_params is missing or not of the right class,
  # construct it interactively via findGroups()
  if (is.null(group_params) || !inherits(group_params, "srtm_group_suggestion")) {
    if (!interactive) {
      rlang::abort(
        "`group_params` must be a 'srtm_group_suggestion' object when `interactive = FALSE`.",
        class = "srtm_assignGroups_missing_group_params"
      )
    }

    rlang::inform(
      "No valid `group_params` supplied. Entering interactive group-finding helper."
    )

    # let the user choose which time variable to use
    cols <- names(data)
    rlang::inform("Select the column to use for grouping (e.g., y1):")
    idx <- utils::menu(cols, graphics = FALSE)
    if (idx == 0) {
      rlang::abort(
        "No column selected for grouping.",
        class = "srtm_assignGroups_no_column"
      )
    }
    time_var <- cols[[idx]]

    group_params <- findGroups(
      data        = data,
      time_var    = time_var,
      interactive = TRUE,
      show_plot   = TRUE
    )
  }

  # --- pull settings from group_params -------------------------------------
  time_name <- group_params$time_var
  nGroups   <- group_params$nGroups
  minima_x  <- sort(group_params$minima_x %||% numeric(0))

  if (!time_name %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{time_name}` not found in `data`."),
      class = "srtm_assignGroups_missing_col"
    )
  }

  x <- data[[time_name]]

  if (!is.numeric(x)) {
    rlang::abort(
      glue::glue("Column `{time_name}` must be numeric."),
      class = "srtm_assignGroups_non_numeric"
    )
  }

  x_valid <- stats::na.omit(x)
  if (length(x_valid) < nGroups) {
    rlang::abort(
      "Not enough non-missing observations to form the requested number of groups.",
      class = "srtm_assignGroups_too_few"
    )
  }

  if (!is.numeric(nGroups) || length(nGroups) != 1L || nGroups < 1L) {
    rlang::abort(
      "`group_params$nGroups` must be a single positive integer.",
      class = "srtm_assignGroups_bad_nGroups"
    )
  }

  nGroups <- as.integer(nGroups)
  if (nGroups > length(LETTERS)) {
    rlang::abort(
      glue::glue(
        "`nGroups` cannot exceed {length(LETTERS)} (A-Z)."
      ),
      class = "srtm_assignGroups_too_many"
    )
  }

  n_minima <- length(minima_x)

  if (nGroups == 1L) {
    groups <- factor(rep("A", length(x)), levels = LETTERS[1])
    return(groups)
  }

  if (n_minima < (nGroups - 1L)) {
    rlang::abort(
      glue::glue(
        "Cannot assign {nGroups} groups: only {n_minima} local minima stored in `group_params` (need at least {nGroups - 1})."
      ),
      class = "srtm_assignGroups_not_enough_minima"
    )
  }

  # if more minima than needed, spread cutpoints across them
  if (n_minima > (nGroups - 1L)) {
    idx <- unique(round(seq(1, n_minima, length.out = nGroups - 1L)))
    cutpoints <- minima_x[idx]
  } else {
    cutpoints <- minima_x
  }

  breaks <- c(-Inf, cutpoints, Inf)

  groups <- cut(
    x,
    breaks          = breaks,
    labels          = LETTERS[seq_len(nGroups)],
    include.lowest  = TRUE,
    right           = TRUE,
    ordered_result  = TRUE
  )

  groups
}
