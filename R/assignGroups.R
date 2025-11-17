assignGroups <- function(data,
                         group_params = NULL,
                         interactive  = TRUE,
                         method       = c("density", "kmeans")) {

  # track whether user explicitly supplied `method`
  method_missing <- missing(method)

  # basic validation / default for method
  if (!method_missing) {
    method <- rlang::arg_match(method)
  } else {
    method <- "density"
  }

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
  time_name        <- group_params$time_var
  nGroups          <- group_params$nGroups
  suggested_nGroups <- group_params$suggested_nGroups %||% NA_integer_
  minima_x         <- sort(group_params$minima_x %||% numeric(0))

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

  x_valid <- stats::na.omit(x)
  if (length(x_valid) < nGroups) {
    rlang::abort(
      "Not enough non-missing observations to form the requested number of groups.",
      class = "srtm_assignGroups_too_few"
    )
  }

  # --- if user overrode suggested_nGroups, optionally choose method --------
  if (!is.na(suggested_nGroups) &&
      suggested_nGroups != nGroups &&
      interactive &&
      method_missing) {

    rlang::inform(
      glue::glue(
        "You have chosen {nGroups} groups, but the suggested number was {suggested_nGroups}."
      )
    )
    choice <- utils::menu(
      choices = c("Use density-based minima", "Use k-means clustering"),
      title   = "How would you like to assign groups?"
    )

    if (choice == 0) {
      rlang::abort(
        "No method selected for assigning groups.",
        class = "srtm_assignGroups_no_method"
      )
    }

    method <- if (choice == 1L) "density" else "kmeans"

    rlang::inform(glue::glue("Using `{method}` method to assign groups."))
  }

  if (nGroups == 1L) {
    groups <- factor(rep("A", length(x)), levels = LETTERS[1])
    return(groups)
  }

  # --- define cutpoints depending on method --------------------------------
  if (identical(method, "density")) {
    n_minima <- length(minima_x)

    if (n_minima < (nGroups - 1L)) {
      rlang::abort(
        glue::glue(
          "Cannot assign {nGroups} groups: only {n_minima} local minima stored in `group_params` (need at least {nGroups - 1})."
        ),
        class = "srtm_assignGroups_not_enough_minima"
      )
    }

    if (n_minima > (nGroups - 1L)) {
      idx <- unique(round(seq(1, n_minima, length.out = nGroups - 1L)))
      cutpoints <- minima_x[idx]
    } else {
      cutpoints <- minima_x
    }

  } else { # method == "kmeans"
    # require at least nGroups distinct values
    if (length(unique(x_valid)) < nGroups) {
      rlang::abort(
        glue::glue(
          "Cannot assign {nGroups} k-means groups: only {length(unique(x_valid))} distinct values in `{time_name}`."
        ),
        class = "srtm_assignGroups_kmeans_not_enough_distinct"
      )
    }

    # run k-means with at least 10 random starts to stabilise centres
    km <- stats::kmeans(
      x_valid,
      centers = nGroups,
      nstart  = max(10L, nGroups)
    )

    centers <- sort(as.numeric(km$centers))

    # midpoints between adjacent centres become cutpoints
    cutpoints <- (centers[-1] + centers[-length(centers)]) / 2
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
