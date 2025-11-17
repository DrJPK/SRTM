#' Find latent groups at a given time point using kernel density estimation
#'
#' @description
#' Uses kernel density estimation (KDE) on a single time point (e.g. baseline
#' `y1`) to suggest how many latent groups may be present in the data. Local
#' minima of the estimated density are treated as potential boundaries between
#' groups. The function can optionally show a diagnostic density plot and allow
#' the user to confirm or override the suggested number of groups.
#'
#' @param data A data frame or tibble containing at least one numeric column
#'   representing the time point of interest (e.g. `y1`).
#' @param time_var A column specification (name or tidy-selection) indicating
#'   which variable in `data` should be used for group finding. Defaults to
#'   `"y1"`.
#' @param interactive Logical. If `TRUE` (default), the user is given the
#'   opportunity to override the suggested number of groups via a console
#'   prompt.
#' @param show_plot Logical. If `TRUE`, a `ggplot2` density plot is printed
#'   showing the KDE curves for multiple bandwidth adjustments and the local
#'   minima for the main adjustment. Defaults to the value of `interactive`.
#' @param bw Bandwidth argument passed to [stats::density()]. Can be a numeric
#'   bandwidth or a character string such as `"nrd0"` (the default).
#' @param adjust Numeric scaling factor for the KDE bandwidth passed to
#'   [stats::density()]. Values greater than 1 give smoother curves, values
#'   less than 1 give more detailed curves.
#' @param adjust_grid Numeric vector of additional `adjust` values used for
#'   bandwidth diagnostics. KDEs are fitted for each value in `adjust_grid`
#'   and overlaid in the plot (if `show_plot = TRUE`), and a small summary of
#'   detected minima and suggested group counts is returned.
#'
#' @details
#' For the selected `time_var`, the function:
#'
#' \enumerate{
#'   \item Computes a kernel density estimate using [stats::density()] with the
#'     specified `bw` and `adjust`.
#'   \item Approximates the derivative of the density and identifies local
#'     minima (points where the derivative changes sign from negative to
#'     positive).
#'   \item Uses the number of local minima to propose a suggested number of
#'     groups: if there are \\eqn{k} minima, \\eqn{k + 1} groups are suggested;
#'     if no minima are found, a single group is suggested.
#'   \item Repeats the KDE/minima detection for each value in `adjust_grid` to
#'     provide a bandwidth-sensitivity diagnostic.
#' }
#'
#' When `show_plot = TRUE` and `ggplot2` is available, the function prints a
#' density plot for all adjustments in `adjust_grid`, coloured by `adjust`,
#' with dashed vertical lines marking the local minima for the main `adjust`
#' value. This allows users to visually inspect how stable the suggested group
#' structure is with respect to bandwidth choices.
#'
#' If `interactive = TRUE`, the user is prompted in the console to accept the
#' suggested number of groups or enter a different value. The final choice is
#' stored in the returned object.
#'
#' @return
#' An object of class `"srtm_group_suggestion"` (a list) with elements:
#'
#' \itemize{
#'   \item `nGroups` — the final number of groups to use (possibly modified by the user).
#'   \item `suggested_nGroups` — the initial number of groups suggested from the main KDE.
#'   \item `minima_x` — numeric vector of local minima positions (for the main `adjust`).
#'   \item `time_var` — character string naming the variable used for grouping.
#'   \item `density` — the main [stats::density()] object for the chosen `bw` and `adjust`.
#'   \item `bw` — the bandwidth argument used.
#'   \item `adjust` — the main bandwidth adjustment factor.
#'   \item `bw_diagnostic` — a tibble summarising, for each value in
#'     `adjust_grid`, the number of minima, suggested number of groups, and
#'     the corresponding minima locations.
#' }
#'
#' This object is intended to be passed to [assignGroups()] to perform the
#' actual group assignment.
#'
#' @examples
#' # Using synthetic data
#' df <- generateSynthData(n = 300, seed = 123)
#'
#' # Non-interactive: get suggested groups for y1
#' gp <- findGroups(
#'   data        = df,
#'   time_var    = "y1",
#'   interactive = FALSE,
#'   show_plot   = FALSE
#' )
#'
#' gp$nGroups
#' gp$bw_diagnostic
#'
#' \dontrun{
#' # Interactive, with plot, suitable for teacher-facing workflows
#' gp <- findGroups(df, time_var = "y1", interactive = TRUE, show_plot = TRUE)
#'
#' # Use the result to assign groups later:
#' df$Group <- assignGroups(df, group_params = gp)
#' }
#'
#' @export

findGroups <- function(data,
                       time_var    = "y1",
                       interactive = TRUE,
                       show_plot   = interactive,
                       bw          = "nrd0",
                       adjust      = 1,
                       adjust_grid = c(0.5, 1, 2)) {

  # --- basic checks --------------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_findGroups_bad_data"
    )
  }

  if (is.character(time_var) && length(time_var) == 1L) {
    time_name <- time_var
  } else {
    time_sym  <- rlang::ensym(time_var)
    time_name <- rlang::as_string(time_sym)
  }

  if (!time_name %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{time_name}` not found in `data`."),
      class = "srtm_findGroups_missing_col"
    )
  }

  x <- data[[time_name]]

  if (!is.numeric(x)) {
    rlang::abort(
      glue::glue("Column `{time_name}` must be numeric."),
      class = "srtm_findGroups_non_numeric"
    )
  }

  x <- stats::na.omit(x)
  if (length(x) < 5L) {
    rlang::abort(
      "Not enough non-missing observations to estimate density.",
      class = "srtm_findGroups_too_few"
    )
  }

  # --- helper to compute minima + suggestion for a given adjust -----------
  find_minima <- function(x, bw, adjust) {
    dens <- stats::density(x, bw = bw, adjust = adjust, na.rm = TRUE)
    y <- dens$y

    dy <- diff(y)
    sign_dy <- sign(dy)
    sign_dy[sign_dy == 0] <- NA

    minima_idx <- which(
      head(sign_dy, -1) < 0 & tail(sign_dy, -1) > 0
    ) + 1L

    minima_x    <- dens$x[minima_idx]
    n_minima    <- length(minima_x)
    suggested_n <- if (n_minima == 0L) 1L else n_minima + 1L

    list(
      density      = dens,
      minima_idx   = minima_idx,
      minima_x     = minima_x,
      n_minima     = n_minima,
      suggested_n  = suggested_n
    )
  }

  # ensure main adjust is included in the grid
  adjust_grid <- unique(sort(c(adjust, adjust_grid)))

  # compute results for all adjust values
  res_list <- purrr::map(adjust_grid, \(a) find_minima(x, bw = bw, adjust = a))

  # locate the main adjust entry
  main_idx   <- which(adjust_grid == adjust)[1L]
  main_res   <- res_list[[main_idx]]
  dens_main  <- main_res$density
  minima_x   <- main_res$minima_x
  n_minima   <- main_res$n_minima
  suggested_n <- main_res$suggested_n

  # bandwidth diagnostic tibble
  bw_diag <- purrr::map2_df(adjust_grid, res_list, \(a, res_a) {
    tibble::tibble(
      adjust      = a,
      n_minima    = res_a$n_minima,
      suggested_n = res_a$suggested_n,
      minima_list = list(res_a$minima_x)
    )
  })

  # combined density data for plotting
  dens_all_df <- purrr::map2_df(adjust_grid, res_list, \(a, res_a) {
    tibble::tibble(
      x      = res_a$density$x,
      y      = res_a$density$y,
      adjust = factor(a)
    )
  })

  # --- optional plotting ---------------------------------------------------
  if (isTRUE(show_plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      rlang::warn(
        "Package `ggplot2` not installed; cannot show plot.",
        class = "srtm_findGroups_no_ggplot2"
      )
    } else {
      minima_df <- tibble::tibble(
        x = minima_x,
        y = dens_main$y[main_res$minima_idx]
      )

      p <- ggplot2::ggplot(dens_all_df, ggplot2::aes(x = x, y = y, colour = adjust)) +
        ggplot2::geom_line() +
        # highlight minima for the *main* adjust
        ggplot2::geom_vline(
          xintercept = minima_x,
          linetype   = "dashed"
        ) +
        ggplot2::geom_point(
          data = minima_df,
          ggplot2::aes(x = x, y = y),
          inherit.aes = FALSE
        ) +
        ggplot2::labs(
          x = time_name,
          y = "Density",
          colour = "Value of\nadjust",
          title = glue::glue(
            "Density of {time_name} for different bandwidth adjustments"
          ),
          subtitle = glue::glue(
            "Main adjust = {adjust}; detected {n_minima} local minima; suggested groups: {suggested_n}"
          )
        ) +
        ggplot2::theme_minimal()

      print(p)

      rlang::inform(
        paste0(
          "Bandwidth diagnostic (bw = ", bw, "):\n",
          paste(
            sprintf(
              "  adjust = %g: minima = %d, suggested groups = %d",
              bw_diag$adjust, bw_diag$n_minima, bw_diag$suggested_n
            ),
            collapse = "\n"
          )
        ),
        class = "srtm_findGroups_bw_diagnostic"
      )
    }
  } else {
    # console-only summary if no plot
    if (n_minima == 0L) {
      rlang::inform(
        glue::glue(
          "No clear local minima detected for `{time_name}`; suggesting 1 group."
        ),
        class = "srtm_findGroups_no_minima"
      )
    } else {
      rlang::inform(
        glue::glue(
          "Detected {n_minima} local minima in `{time_name}` at: {paste(round(minima_x, 2), collapse = ', ')}.\n",
          "Suggested number of groups: {suggested_n}."
        ),
        class = "srtm_findGroups_summary"
      )
    }

    rlang::inform(
      paste0(
        "Bandwidth diagnostic (bw = ", bw, "):\n",
        paste(
          sprintf(
            "  adjust = %g: minima = %d, suggested groups = %d",
            bw_diag$adjust, bw_diag$n_minima, bw_diag$suggested_n
          ),
          collapse = "\n"
        )
      ),
      class = "srtm_findGroups_bw_diagnostic"
    )
  }

  # --- interactive override of nGroups -------------------------------------
  final_n <- suggested_n

  if (isTRUE(interactive)) {
    prompt <- glue::glue(
      "Suggested number of groups (for adjust = {adjust}) is {suggested_n}. ",
      "Press Enter to accept or type a different number: "
    )
    ans <- readline(prompt)

    if (nzchar(ans)) {
      n_user <- suppressWarnings(as.integer(ans))
      if (is.na(n_user) || n_user < 1L) {
        rlang::warn(
          "Invalid input; keeping suggested number of groups.",
          class = "srtm_findGroups_bad_input"
        )
      } else {
        final_n <- n_user
      }
    }
  }

  # --- return object -------------------------------------------------------
  res <- list(
    nGroups           = final_n,
    suggested_nGroups = suggested_n,
    minima_x          = minima_x,
    time_var          = time_name,
    density           = dens_main,
    bw                = bw,
    adjust            = adjust,
    bw_diagnostic     = bw_diag
  )

  class(res) <- c("srtm_group_suggestion", class(res))
  res
}

