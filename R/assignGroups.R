#' Assign SRMT trajectory groups from a single time-point
#'
#' `assignGroups()` takes a numeric time-point variable (e.g., \code{y1}) from a
#' data frame and assigns each case to an ordered group (A, B, C, …) based on
#' either density minima or k-means clustering. It is designed to work with
#' group suggestions produced by \code{\link{findGroups}()} and is used inside
#' Self-Referenced Trajectory Modelling (SRMT) workflows to create discrete
#' trajectory groups from continuous scores.
#'
#' In typical usage, the user first calls \code{findGroups()} to interactively
#' explore the distribution of a time-point (e.g., \code{y1}) and to choose an
#' appropriate number of groups and cutpoints. The resulting
#' \code{"srtm_group_suggestion"} object is then supplied to
#' \code{assignGroups()}, which applies these settings to the data and returns
#' an ordered factor of group labels for each case.
#'
#' If \code{group_params} is not supplied (or is not a
#' \code{"srtm_group_suggestion"}) and \code{interactive = TRUE}, the function
#' will prompt the user to select a column from \code{data} and will call
#' \code{findGroups()} interactively to build \code{group_params} on the fly.
#'
#' @param data A data frame or tibble containing at least one numeric column
#'   from which groups are to be derived (e.g., \code{y1}). The column actually
#'   used is determined by \code{group_params$time_var}, or selected
#'   interactively if \code{group_params} is not provided and
#'   \code{interactive = TRUE}.
#'
#' @param group_params An object of class \code{"srtm_group_suggestion"}
#'   typically returned by \code{\link{findGroups}()}. At minimum, this object
#'   should contain:
#'   \itemize{
#'     \item \code{time_var}: the name of the numeric column in \code{data} to
#'       use for grouping (e.g., \code{"y1"}).
#'     \item \code{nGroups}: the desired number of groups.
#'     \item \code{minima_x}: a numeric vector of local minima locations (used
#'       when \code{method = "density"}).
#'     \item \code{preferred_method}: optional; a character string
#'       \code{"density"} or \code{"kmeans"} recommended by
#'       \code{findGroups()}.
#'   }
#'   If \code{group_params} is \code{NULL} or not of the correct class, and
#'   \code{interactive = FALSE}, the function will abort with an error.
#'
#' @param interactive Logical. If \code{TRUE} (the default), the function is
#'   allowed to fall back to an interactive helper when a valid
#'   \code{group_params} object is not supplied: the user is presented with a
#'   menu of column names and \code{findGroups()} is called to construct
#'   \code{group_params}. If \code{FALSE}, a valid \code{group_params} must be
#'   supplied and no interactive prompts are shown.
#'
#' @param method Character string specifying how to derive cutpoints when
#'   assigning groups. One of \code{"density"} or \code{"kmeans"}. If
#'   \code{method} is missing, the function will:
#'   \itemize{
#'     \item use \code{group_params$preferred_method}, if present and valid;
#'     \item otherwise default to \code{"density"}.
#'   }
#'   If \code{method} is supplied explicitly, it overrides any preference stored
#'   in \code{group_params}.
#'
#' @param label_scheme Character string specifying the labelling scheme to be
#'   used for groups. One of \code{"letters"}, \code{"signs"} or
#'   \code{"arrows"}. Currently, only \code{"letters"} (A, B, C, …) is used
#'   internally; the argument is included for future extension of labelling
#'   conventions.
#'
#' @return An ordered factor of length \code{nrow(data)} giving the group
#'   membership for each row. By default, groups are labelled with uppercase
#'   letters (\code{"A"}, \code{"B"}, \code{"C"}, …). The levels are ordered
#'   such that:
#'   \itemize{
#'     \item \code{"A"} corresponds to the group with the highest mean value on
#'       the selected time variable;
#'     \item subsequent letters (\code{"B"}, \code{"C"}, …) correspond to
#'       progressively lower mean values.
#'   }
#'   Observations with \code{NA} in the selected time variable receive
#'   \code{NA} group assignments.
#'
#' @details
#' When \code{method = "density"}, \code{assignGroups()} uses the local minima
#' stored in \code{group_params$minima_x} as potential cutpoints. If there are
#' more minima than required for \code{nGroups - 1} cutpoints, a subset of
#' minima is selected along the range to approximate evenly spaced valleys.
#'
#' When \code{method = "kmeans"}, the function runs a univariate k-means
#' clustering on the non-missing values of the selected time variable, using
#' \code{nGroups} centres and at least 10 random starts. The midpoints between
#' adjacent cluster centres are used as cutpoints.
#'
#' In both cases, the numeric variable is first split into intervals using
#' \code{\link[base]{cut}()}, and the resulting groups are then re-labelled so
#' that the group with the highest mean score is labelled \code{"A"}, the next
#' highest \code{"B"}, and so on, ensuring that “higher” groups always
#' correspond to higher average scores.
#'
#' @seealso \code{\link{findGroups}()} for constructing
#'   \code{"srtm_group_suggestion"} objects interactively, and
#'   \code{\link{SRTMAnalyse}()} for higher-level SRMT modelling workflows that
#'   make use of group assignments.
#'
#' @examples
#' \dontrun{
#' # Simple example using a single time-point column y1
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   y1 = rnorm(200, mean = 50, sd = 10)
#' )
#'
#' # Interactively explore and choose groups, then assign them
#' gp <- findGroups(
#'   data        = df,
#'   time_var    = "y1",
#'   interactive = TRUE,
#'   show_plot   = TRUE
#' )
#'
#' df$group <- assignGroups(
#'   data         = df,
#'   group_params = gp
#' )
#'
#' # Non-interactive usage: require a valid group_params object
#' df$group_km <- assignGroups(
#'   data         = df,
#'   group_params = gp,
#'   interactive  = FALSE,
#'   method       = "kmeans"
#' )
#' }
#'
#' @export

assignGroups <- function(data,
                         group_params = NULL,
                         interactive  = TRUE,
                         method       = c("density", "kmeans"),
                         label_scheme = c("letters", "signs", "arrows")) {

  label_scheme <- rlang::arg_match(label_scheme)

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
  time_name         <- group_params$time_var
  nGroups           <- group_params$nGroups
  suggested_nGroups <- group_params$suggested_nGroups %||% NA_integer_
  minima_x          <- sort(group_params$minima_x %||% numeric(0))

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

  # --- decide method: explicit argument > preference from findGroups > default
  if (missing(method)) {
    if (!is.null(group_params$preferred_method)) {
      pm <- group_params$preferred_method
      if (!is.character(pm) || length(pm) != 1L ||
          !pm %in% c("density", "kmeans")) {
        rlang::warn(
          "Ignoring invalid `preferred_method` in `group_params`; defaulting to 'density'.",
          class = "srtm_assignGroups_bad_preferred_method"
        )
        method <- "density"
      } else {
        method <- pm
        rlang::inform(
          glue::glue("Using `{method}` method as chosen in findGroups()."),
          class = "srtm_assignGroups_use_preferred_method"
        )
      }
    } else {
      method <- "density"
    }
  } else {
    method <- rlang::arg_match(method)
  }

  # refresh minima_x (in case group_params was modified upstream)
  minima_x <- sort(group_params$minima_x %||% numeric(0))

  # --- trivial single-group case ------------------------------------------
  if (nGroups == 1L) {
    groups <- factor(rep("A", length(x)), levels = LETTERS[1], ordered = TRUE)
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

  # Make sure group A corresponds to the rightmost group
  group_means <- tapply(x, groups, mean, na.rm = TRUE)
  ord         <- order(group_means, decreasing = TRUE)
  old_levels  <- levels(groups)
  new_levels  <- old_levels[ord]
  new_labels  <- LETTERS[seq_len(length(new_levels))]

  groups <- factor(
    groups,
    levels  = new_levels,
    labels  = new_labels,
    ordered = TRUE
  )

  groups
}
