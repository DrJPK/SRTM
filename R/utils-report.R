#### ---- SIMPLE CUTPLOT HELPER FOR REPORTING

#' Build a cut-point plot for inclusion in an SRTM report
#'
#' @keywords internal
srtm_build_cutpoint_plot <- function(results,
                                     group_first = c("slope", "baseline"),
                                     plot_palette = "simple",
                                     debug = FALSE) {

  group_first <- rlang::arg_match(group_first)

  df       <- results$data
  settings <- results$settings
  y1_name  <- settings$y1 %||% "y1"

  if (debug) {
    rlang::inform(
      glue::glue(
        "srtm_build_cutpoint_plot(): group_first = '{group_first}', y1_name = '{y1_name}', palette = '{plot_palette}'."
      ),
      class = "srtm_report_debug"
    )
  }

  if (identical(group_first, "slope")) {
    # Show trajectory cut-points on m01 (overall)
    which_label <- "Trajectory (overall)"

    if (debug) {
      rlang::inform(
        glue::glue(
          "srtm_build_cutpoint_plot(): using '{which_label}' from results$group_params."
        ),
        class = "srtm_report_debug"
      )
    }

    p <- plotSRTMCutPoints(
      x       = results,
      which   = which_label,
      palette = plot_palette
    )

    traj_lab_col <- if ("trajType" %in% names(df)) "trajType" else "trajGroup"

    lab_df <- df %>%
      dplyr::filter(!is.na(.data[[traj_lab_col]])) %>%
      dplyr::group_by(traj = .data[[traj_lab_col]]) %>%
      dplyr::summarise(
        x = mean(.data$m01, na.rm = TRUE),
        .groups = "drop"
      )

    pal_traj <- setPlotPalette(plot_palette)
    traj_levels <- as.character(lab_df$traj)
    traj_cols   <- rep(pal_traj$colours, length.out = length(traj_levels))
    names(traj_cols) <- traj_levels

    p <- p +
      ggplot2::geom_text(
        data  = lab_df,
        ggplot2::aes(
          x     = x,
          y     = 0,
          label = traj,
          colour = traj
        ),
        vjust = -0.7,
        size  = 6   # <-- made labels larger
      ) +
      ggplot2::scale_colour_manual(values = traj_cols, guide = "none")

  } else {
    # Baseline-first: show overall baseline cut-points on y1
    which_label <- "Baseline (overall)"

    if (debug) {
      rlang::inform(
        glue::glue(
          "srtm_build_cutpoint_plot(): using '{which_label}' from results$group_params."
        ),
        class = "srtm_report_debug"
      )
    }

    p <- plotSRTMCutPoints(
      x       = results,
      which   = which_label,
      palette = plot_palette
    )

    lab_df <- df %>%
      dplyr::filter(!is.na(.data$baseGroup)) %>%
      dplyr::group_by(baseGroup) %>%
      dplyr::summarise(
        x = mean(.data[[y1_name]], na.rm = TRUE),
        .groups = "drop"
      )

    pal_base <- setPlotPalette(plot_palette)
    base_levels <- as.character(lab_df$baseGroup)
    base_cols   <- rep(pal_base$colours, length.out = length(base_levels))
    names(base_cols) <- base_levels

    p <- p +
      ggplot2::geom_text(
        data  = lab_df,
        ggplot2::aes(
          x      = x,
          y      = 0,
          label  = baseGroup,
          colour = baseGroup
        ),
        vjust = -0.7,
        size  = 6   # <-- made labels larger
      ) +
      ggplot2::scale_colour_manual(values = base_cols, guide = "none")
  }

  if (debug) {
    rlang::inform(
      "srtm_build_cutpoint_plot(): cut-point plot built successfully.",
      class = "srtm_report_debug"
    )
  }

  p
}

#### ---- COMBINED PLOT HELPER FOR REPORTING

#' Build a combined cut-point plot for reporting
#'
#' @description
#' Constructs a patchwork layout combining:
#' \itemize{
#'   \item a large top panel showing the \emph{first} grouping
#'         (baseline or trajectory, depending on \code{group_first}), and
#'   \item smaller panels showing the \emph{secondary} groupings
#'         within each primary group (e.g., trajectories within each
#'         baseline group, or baselines within each trajectory type).
#' }
#'
#' The layout aims to place the main panel across the top row, with
#' secondary panels arranged underneath. For example, when there are
#' three primary groups:
#'
#' \preformatted{
#'   1 1 1
#'   2 3 4
#' }
#'
#' where panel 1 is the overall cut-points, and 2/3/4 are secondary
#' panels for the lowest, middle, and highest primary groups (based on
#' their mean score on the grouping variable).
#'
#' @param results An object of class \code{"srtm_analysis"} as returned
#'   by \code{SRTMAnalyse()}.
#' @param plot_palette Character palette name passed to
#'   \code{\link{setPlotPalette}} (default \code{"simple"}).
#' @param debug Logical; if \code{TRUE}, emits informative progress
#'   messages via \code{rlang::inform()}.
#'
#' @return A patchwork object (i.e., a \code{ggplot} object with
#'   patchwork layout) combining the main cut-point plot and the
#'   secondary plots. Returns \code{NULL} if the required group
#'   suggestions are not available.
#'
#' @keywords internal
srtm_build_combined_cutpoint_plot <- function(results,
                                              plot_palette = c("simple", "pastel", "colourblind", "greys"),
                                              debug = FALSE) {

  plot_palette <- rlang::arg_match(plot_palette)

  if (!inherits(results, "srtm_analysis")) {
    rlang::abort(
      "`results` must be an object of class 'srtm_analysis'.",
      class = "srtm_report_bad_results"
    )
  }

  df       <- results$data
  settings <- results$settings
  gp       <- results$group_params %||% list()

  y1_name <- settings$y1 %||% "y1"

  # Canonical group variable names
  gv <- srtm_resolve_group_vars(results, debug = debug)

  group_first         <- gv$group_first
  primary_group_var   <- gv$primary_group_var
  secondary_group_var <- gv$secondary_group_var
  base_group_var      <- gv$base_col
  traj_group_var      <- gv$traj_col

  if (debug) {
    rlang::inform(
      glue::glue(
        "srtm_build_combined_cutpoint_plot(): group_first = '{group_first}', palette = '{plot_palette}'."
      ),
      class = "srtm_report_debug"
    )
  }

  # --------------------------------------------------------------------------
  # Decide which group suggestion is "main" and which set is "secondary"
  # --------------------------------------------------------------------------
  # if (identical(group_first, "baseline")) {
  #   gp_main    <- gp$baseline_overall
  #   sec_list   <- gp$traj_by_base %||% list()
  #   primary_var    <- y1_name   # baseline scores
  #   primary_label  <- glue::glue("Baseline score ({y1_name})")
  #   secondary_label <- "Historical slope (m01)"
  #
  #   primary_group_var <- "baseGroup"
  #   secondary_group_var <- "trajGroup"
  #
  # } else {  # group_first == "slope"
  #   gp_main    <- gp$traj_overall
  #   sec_list   <- gp$baseline_by_traj %||% list()
  #   primary_var    <- "m01"      # slopes
  #   primary_label  <- "Historical slope (m01)"
  #   secondary_label <- glue::glue("Baseline score ({y1_name})")
  #
  #   primary_group_var   <- "trajGroup"
  #   secondary_group_var <- "baseGroup"
  # }

  if (identical(group_first, "baseline")) {
    gp_main        <- gp$baseline_overall
    sec_list       <- gp$traj_by_base %||% list()
    primary_var    <- y1_name            # baseline scores
    primary_label  <- glue::glue("Baseline score ({y1_name})")
    secondary_label <- "Historical slope (m01)"
  } else {  # group_first == "slope"
    gp_main        <- gp$traj_overall
    sec_list       <- gp$baseline_by_traj %||% list()
    primary_var    <- "m01"              # slopes
    primary_label  <- "Historical slope (m01)"
    secondary_label <- glue::glue("Baseline score ({y1_name})")
  }

  if (is.null(gp_main) || !inherits(gp_main, "srtm_group_suggestion")) {
    if (debug) {
      rlang::inform(
        "srtm_build_combined_cutpoint_plot(): no usable main group suggestion found; returning NULL.",
        class = "srtm_report_debug"
      )
    }
    return(NULL)
  }

  if (length(sec_list) == 0L) {
    if (debug) {
      rlang::inform(
        "srtm_build_combined_cutpoint_plot(): no secondary group suggestions found; returning single main plot.",
        class = "srtm_report_debug"
      )
    }
    # Single plot fallback
    return(
      plotCutPoints(
        group_params = gp_main,
        title_var    = if (identical(group_first, "baseline")) "Baseline (overall)" else "Trajectory (overall)",
        x_label      = primary_label,
        palette      = plot_palette
      )
    )
  }

  # --------------------------------------------------------------------------
  # Build main plot (panel 1)
  # --------------------------------------------------------------------------
  main_title <- if (identical(group_first, "baseline")) {
    "Baseline grouping (overall)"
  } else {
    "Trajectory grouping (overall)"
  }

  p_main <- plotCutPoints(
    group_params = gp_main,
    title_var    = main_title,
    x_label      = primary_label,
    palette      = plot_palette
  )

  p_main <- srtm_add_group_overlay(
    p_main,
    df        = df,
    group_var = primary_group_var,
    x_field   = primary_var,
    palette   = plot_palette,
    size      = 4.5
  )

  # Wrap and shrink the main panel title so it doesn't overflow
  p_main <- srtm_wrap_title(p_main, width = 32, size = 11)

  # --------------------------------------------------------------------------
  # Determine ordering of primary groups (lowest -> highest) so that
  # panel 2 = lowest, last panel = highest.
  # --------------------------------------------------------------------------
  if (!primary_group_var %in% names(df)) {
    if (debug) {
      rlang::inform(
        glue::glue(
          "srtm_build_combined_cutpoint_plot(): primary group variable `{primary_group_var}` not found in data; using list order for secondary panels."
        ),
        class = "srtm_report_debug"
      )
    }
    ordered_sec_names <- names(sec_list)
  } else {
    # Metric for ordering: y1 for baseline-first, m01 for slope-first
    metric_name <- if (identical(group_first, "baseline")) y1_name else "m01"

    if (!metric_name %in% names(df)) {
      if (debug) {
        rlang::inform(
          glue::glue(
            "srtm_build_combined_cutpoint_plot(): metric `{metric_name}` not found; using list order for secondary panels."
          ),
          class = "srtm_report_debug"
        )
      }
      ordered_sec_names <- names(sec_list)
    } else {
      order_tbl <- df %>%
        dplyr::filter(!is.na(.data[[primary_group_var]])) %>%
        dplyr::group_by(.data[[primary_group_var]]) %>%
        dplyr::summarise(
          mean_val = mean(.data[[metric_name]], na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        dplyr::arrange(.data$mean_val)

      # Only keep groups for which we actually have a secondary suggestion
      ordered_sec_names <- as.character(order_tbl[[primary_group_var]])
      ordered_sec_names <- intersect(ordered_sec_names, names(sec_list))

      if (length(ordered_sec_names) == 0L) {
        ordered_sec_names <- names(sec_list)
      }

      if (debug) {
        rlang::inform(
          glue::glue(
            "srtm_build_combined_cutpoint_plot(): ordered primary groups = {paste(ordered_sec_names, collapse = ', ')}."
          ),
          class = "srtm_report_debug"
        )
      }
    }
  }

  # --------------------------------------------------------------------------
  # Build secondary plots (one per primary group, in ordered_sec_names)
  # --------------------------------------------------------------------------
  sec_plots <- lapply(ordered_sec_names, function(nm) {
    gp_i <- sec_list[[nm]]
    if (is.null(gp_i) || !inherits(gp_i, "srtm_group_suggestion")) {
      if (debug) {
        rlang::inform(
          glue::glue(
            "srtm_build_combined_cutpoint_plot(): skipping secondary group '{nm}' (no valid srtm_group_suggestion)."
          ),
          class = "srtm_report_debug"
        )
      }
      return(NULL)
    }

    title_i <- if (identical(group_first, "baseline")) {
      glue::glue("Trajectories within baseline {nm}")
    } else {
      glue::glue("Baselines within trajectory {nm}")
    }

    p_i <- plotCutPoints(
      group_params = gp_i,
      title_var    = title_i,
      x_label      = secondary_label,
      palette      = plot_palette
    )

    p_i <- srtm_add_group_overlay(
      p_i,
      df            = df |> dplyr::filter(.data[[primary_group_var]] == nm),
      group_var     = secondary_group_var,   # e.g., trajGroup or baseGroup
      x_field       = if (identical(group_first, "baseline")) "m01" else y1_name,
      palette       = plot_palette,
      size          = 3.8
    )

    # Wrap and shrink subplot titles to fit their smaller panels
    p_i <- srtm_wrap_title(p_i, width = 24, size = 9)

    p_i
  })

  # Drop NULLs if any
  valid_idx  <- vapply(sec_plots, inherits, logical(1), what = "ggplot")
  sec_plots  <- sec_plots[valid_idx]
  sec_labels <- ordered_sec_names[valid_idx]

  n_sec <- length(sec_plots)

  if (n_sec == 0L) {
    if (debug) {
      rlang::inform(
        "srtm_build_combined_cutpoint_plot(): no valid secondary plots; returning main plot only.",
        class = "srtm_report_debug"
      )
    }
    return(p_main)
  }

  # --------------------------------------------------------------------------
  # Patchwork layout
  # --------------------------------------------------------------------------
  all_plots <- c(list(p_main), sec_plots)

  if (n_sec <= 2L) {
    # layout:
    # n_sec = 1: 1 1
    #           2 NA
    # n_sec = 2: 1 1
    #           2 3
    ncol <- 2L
    top_row <- rep(1L, ncol)

    second_row <- rep(NA_integer_, ncol)
    second_row[seq_len(n_sec)] <- 2:(n_sec + 1L)

    design <- rbind(top_row, second_row)

  } else if (n_sec == 3L) {
    # 1 1 1
    # 2 3 4
    ncol <- 3L
    top_row <- rep(1L, ncol)
    second_row <- matrix(2:4, nrow = 1L)
    design <- rbind(top_row, second_row)

  } else {
    # n_sec >= 4:
    # 1 1 1
    # 2 3 4
    # 5 6 7
    # ...
    ncol <- 3L
    top_row <- rep(1L, ncol)

    remaining <- 2:(n_sec + 1L)
    n_rem     <- length(remaining)
    n_rows_rem <- ceiling(n_rem / ncol)

    fill <- rep(NA_integer_, n_rows_rem * ncol)
    fill[seq_along(remaining)] <- remaining

    rest_mat <- matrix(fill, nrow = n_rows_rem, byrow = TRUE)
    design   <- rbind(top_row, rest_mat)
  }

  # --------------------------------------------------------------------------
  # Convert numeric design matrix to patchwork design string
  # --------------------------------------------------------------------------
  # Each unique integer i in `design` corresponds to the i-th plot in `all_plots`.
  # We map 1 -> "A", 2 -> "B", 3 -> "C", ... and create a string layout
  # like:
  #   "AAA"
  #   "BCD"
  # which patchwork understands.
  # --------------------------------------------------------------------------
  n_plots <- length(all_plots)
  letters_vec <- utils::head(LETTERS, n_plots)

  # Start with all blanks
  letter_mat <- matrix("", nrow = nrow(design), ncol = ncol(design))

  for (i in seq_len(n_plots)) {
    letter_mat[design == i] <- letters_vec[i]
  }

  # Build the design string row-wise
  design_rows <- apply(letter_mat, 1L, paste0, collapse = "")
  design_string <- paste(design_rows, collapse = "\n")

  if (debug) {
    rlang::inform(
      glue::glue(
        "srtm_build_combined_cutpoint_plot(): design (numeric):\n{paste(capture.output(print(design)), collapse = '\n')}\n",
        "srtm_build_combined_cutpoint_plot(): design (string):\n{design_string}"
      ),
      class = "srtm_report_debug"
    )
  }

  patchwork::wrap_plots(all_plots) +
    patchwork::plot_layout(design = design_string) &
    ggplot2::theme(plot.margin = ggplot2::margin(4, 4, 4, 4))
}
