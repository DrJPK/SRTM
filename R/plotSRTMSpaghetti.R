#' Spaghetti plot of SRTM trajectories
#'
#' @description
#' Creates a single-panel spaghetti plot of all observed trajectories.
#' Lines are coloured by ID; points are black. Allows optional dodging
#' of points and optional forcing of discrete vs continuous time on the x-axis.
#'
#' @param data Data frame, typically `results$data` from `SRTMAnalyse()`.
#'   Must contain `y0`, `y1`, `y2`, and an ID column.
#' @param id_col Name of the ID column (default `"ID"`).
#' @param plot_time_discrete Logical:
#'   *TRUE* → always use discrete `"Historical", "Pre", "Post"` x-axis
#'   *FALSE* → use continuous t if `dt01` & `dt12` available, otherwise discrete.
#' @param dodge Logical; if TRUE, apply horizontal dodging to points.
#' @param dodge_width Numeric dodge width (default 0.25).
#'
#' @return A ggplot object.
#' @export
plotSRTMSpaghetti <- function(data,
                              id_col            = "ID",
                              plot_time_discrete = FALSE,
                              dodge              = FALSE,
                              dodge_width        = 0.25) {

  df <- tibble::as_tibble(data)

  if (!id_col %in% names(df)) {
    rlang::abort(
      glue::glue("Column `{id_col}` not found in `data`."),
      class = "srtm_spaghetti_bad_id_col"
    )
  }

  # ---------------------------------------------------------------------------
  # Long format for observed y0, y1, y2
  # ---------------------------------------------------------------------------
  df_long <- df %>%
    tidyr::pivot_longer(
      cols      = tidyselect::all_of(c("y0", "y1", "y2")),
      names_to  = "time",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      time = factor(time, levels = c("y0", "y1", "y2"))
    )

  # Check continuous time availability
  has_time_gaps <- all(c("dt01", "dt12") %in% names(df))
  id_sym <- rlang::sym(id_col)

  # ---------------------------------------------------------------------------
  # Choose x-axis mode
  # ---------------------------------------------------------------------------
  use_discrete <- plot_time_discrete || !has_time_gaps

  if (!use_discrete) {
    # continuous time from t0, t1, t2
    df_long <- df_long %>%
      dplyr::mutate(
        t = dplyr::case_when(
          time == "y0" ~ - .data$dt01,
          time == "y1" ~ 0,
          time == "y2" ~  .data$dt12,
          TRUE         ~ NA_real_
        )
      )

    x_var  <- "t"
    x_lab  <- "Time since baseline"

  } else {
    # discrete labels
    df_long <- df_long %>%
      dplyr::mutate(
        time_label = dplyr::recode(
          .data$time,
          y0 = "Historical",
          y1 = "Pre",
          y2 = "Post"
        )
      )

    x_var  <- "time_label"
    x_lab  <- "Time point"
  }

  # ---------------------------------------------------------------------------
  # Dodge logic
  # ---------------------------------------------------------------------------
  pos <- if (dodge) ggplot2::position_dodge(width = dodge_width) else "identity"

  # ---------------------------------------------------------------------------
  # Build plot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x     = .data[[x_var]],
      y     = .data$value,
      group = !!id_sym
    )
  ) +
    # Lines coloured by ID
    ggplot2::geom_line(
      ggplot2::aes(colour = !!id_sym),
      alpha     = 0.5,
      linewidth = 0.6
    ) +
    # Points in black (optionally dodged)
    ggplot2::geom_point(
      colour   = "black",
      size     = 1.7,
      position = pos
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = x_lab,
      y = "Score"
    ) +
    ggplot2::guides(colour = "none")

  p
}
