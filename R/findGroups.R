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
                       plot_alternatives = FALSE,
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
  main_adjust <-adjust
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

      if(plot_alternatives){
        plot_df <- dens_all_df
      }else{
        plot_df <- dplyr::filter(dens_all_df, .data$adjust == main_adjust)
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y, colour = adjust)) +
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

  # --- interactive choice of nGroups and method ---------------------------
  final_n          <- suggested_n
  preferred_method <- "density"  # default

  if (isTRUE(interactive)) {

    repeat {
      # Main choice: accept or try something else
      choice_main <- utils::menu(
        choices = c("Accept this density-based suggestion",
                    "Try a different grouping strategy"),
        title   = glue::glue(
          "Suggested number of groups (for adjust = {adjust}) is {suggested_n}.\n",
          "Accept this or try something else?"
        )
      )

      if (choice_main == 0L) {
        rlang::abort(
          "No choice made for suggested number of groups.",
          class = "srtm_findGroups_no_n_choice"
        )
      }

      # 1. Accept current suggestion (density-based)
      if (choice_main == 1L) {
        final_n          <- suggested_n
        preferred_method <- "density"
        break
      }

      # 2. Try a different strategy
      method_choice <- utils::menu(
        choices = c(
          "Use k-means clustering (set number of groups directly)",
          "Refine density-based suggestion (try a different `adjust`)"
        ),
        title = "Choose how you want to refine the grouping:"
      )

      if (method_choice == 0L) {
        rlang::warn(
          "No method selected; returning to the main choice.",
          class = "srtm_findGroups_no_method_choice"
        )
        next
      }

      # ---- 2a. K-means: ask for number of groups -------------------------
      if (method_choice == 1L) {
        ans <- readline("Enter the desired number of k-means groups (e.g., 2, 3, 4): ")
        n_user <- suppressWarnings(as.integer(ans))

        if (is.na(n_user) || n_user < 1L) {
          rlang::warn(
            "Invalid number of groups; please enter a positive integer.",
            class = "srtm_findGroups_bad_kmeans_n"
          )
          next
        }

        final_n          <- n_user
        preferred_method <- "kmeans"
        break
      }

      # ---- 2b. Density-refine: loop over adjust values -------------------
      if (method_choice == 2L) {

        repeat {
          current_adjust <- adjust
          rlang::inform(
            glue::glue(
              "Current KDE settings for `{time_name}`: bw = {bw}, adjust = {current_adjust}.\n",
              "Smaller `adjust` reveals more detail (more bumps); larger `adjust` smooths the curve."
            ),
            class = "srtm_findGroups_adjust_info"
          )

          ans_adj <- readline("Enter a new value for `adjust` (e.g., 0.5, 1, 2): ")
          new_adjust <- suppressWarnings(as.numeric(ans_adj))

          if (is.na(new_adjust) || new_adjust <= 0) {
            rlang::warn(
              "Invalid `adjust` value; please enter a positive number.",
              class = "srtm_findGroups_bad_adjust"
            )
            next
          }

          # recompute density + minima for this new adjust
          dens_new <- stats::density(x, bw = bw, adjust = new_adjust, na.rm = TRUE)
          y_new    <- dens_new$y

          dy_new     <- diff(y_new)
          sign_dy    <- sign(dy_new)
          sign_dy[sign_dy == 0] <- NA

          minima_idx_new <- which(
            head(sign_dy, -1) < 0 & tail(sign_dy, -1) > 0
          ) + 1L

          minima_x_new <- dens_new$x[minima_idx_new]
          n_minima_new <- length(minima_x_new)
          suggested_new <- if (n_minima_new == 0L) 1L else n_minima_new + 1L

          # quick plot for this adjust only (if requested and ggplot2 available)
          if (isTRUE(show_plot) && requireNamespace("ggplot2", quietly = TRUE)) {
            minima_df_new <- tibble::tibble(
              x = minima_x_new,
              y = if (length(minima_idx_new) > 0L) dens_new$y[minima_idx_new] else numeric(0)
            )

            p_new <- ggplot2::ggplot(
              tibble::tibble(x = dens_new$x, y = dens_new$y),
              ggplot2::aes(x = x, y = y)
            ) +
              ggplot2::geom_line() +
              ggplot2::geom_vline(
                xintercept = minima_x_new,
                linetype   = "dashed"
              ) +
              ggplot2::geom_point(
                data = minima_df_new,
                ggplot2::aes(x = x, y = y),
                inherit.aes = FALSE
              ) +
              ggplot2::labs(
                x = time_name,
                y = "Density",
                title = glue::glue(
                  "Density of {time_name} for adjust = {new_adjust}"
                ),
                subtitle = glue::glue(
                  "Detected {n_minima_new} minima; suggested groups = {suggested_new}"
                )
              ) +
              ggplot2::theme_minimal()

            print(p_new)
          }

          rlang::inform(
            glue::glue(
              "For adjust = {new_adjust}, detected {n_minima_new} local minima; suggested groups = {suggested_new}."
            ),
            class = "srtm_findGroups_adjust_summary"
          )

          choice_adj <- utils::menu(
            choices = c("Accept this density-based suggestion", "Try another `adjust`"),
            title   = "Do you want to keep this suggestion?"
          )

          if (choice_adj == 0L) {
            rlang::warn(
              "No choice made; returning to method selection.",
              class = "srtm_findGroups_no_adjust_choice"
            )
            break
          }

          if (choice_adj == 1L) {
            # accept this new adjust and its suggestion
            adjust       <- new_adjust
            dens_main    <- dens_new
            minima_x     <- minima_x_new
            n_minima     <- n_minima_new
            suggested_n  <- suggested_new
            final_n      <- suggested_new
            preferred_method <- "density"

            # simple diagnostic table now just for the chosen adjust
            bw_diag <- tibble::tibble(
              adjust      = new_adjust,
              n_minima    = n_minima_new,
              suggested_n = suggested_new,
              minima_list = list(minima_x_new)
            )

            break  # exit adjust-loop
          }

          # else: loop and try another adjust
        }

        # if we have decided, exit outer repeat
        if (!is.null(preferred_method) && !is.na(final_n)) {
          break
        }
      } # end method_choice == 2L
    } # end main repeat
  } # end if interactive

  # --- return object -------------------------------------------------------
  res <- list(
    nGroups           = final_n,
    suggested_nGroups = suggested_n,
    minima_x          = minima_x,
    time_var          = time_name,
    density           = dens_main,
    bw                = bw,
    adjust            = adjust,
    bw_diagnostic     = bw_diag,
    preferred_method  = preferred_method
  )

  class(res) <- c("srtm_group_suggestion", class(res))
  res
}

