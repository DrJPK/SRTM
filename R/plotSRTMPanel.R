plotSRTMPanel <- function(data,
                          facet      = FALSE,
                          show_means = TRUE,
                          show_ci    = FALSE,
                          ci_level   = 0.95,
                          palette    = "simple") {
  # basic checks
  required_cols <- c("ID", "y0", "y1", "y2", "exp_y2",
                     "baseGroup", "trajGroup")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue("Missing required columns in `data`: {paste(missing_cols, collapse = ', ')}."),
      class = "srtm_plot_missing_cols"
    )
  }

  pal <- setPlotPalette(palette)

  # --- observed trajectories ---
  df_obs <- data %>%
    tidyr::pivot_longer(
      cols      = c("y0", "y1", "y2"),
      names_to  = "time",
      values_to = "value"
    ) %>%
    dplyr::mutate(model = "observed")

  # --- expected trajectories: anchor at y1, end at exp_y2 ---
  df_exp <- data %>%
    dplyr::select(ID, y1, exp_y2, baseGroup, trajGroup) %>%
    tidyr::pivot_longer(
      cols      = c("y1", "exp_y2"),
      names_to  = "time_raw",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      model = "expected",
      time  = dplyr::case_when(
        time_raw == "y1"     ~ "y1",
        time_raw == "exp_y2" ~ "y2"
      )
    ) %>%
    dplyr::select(-time_raw)

  # --- combine & factorise ---
  df <- dplyr::bind_rows(df_obs, df_exp) %>%
    dplyr::mutate(
      time  = factor(time, levels = c("y0", "y1", "y2")),
      model = factor(model, levels = c("observed", "expected"))
    )

  # --- compute group summaries (means & CIs) -------------------------------
  if (show_means || show_ci) {
    summary_df <- df %>%
      dplyr::group_by(baseGroup, trajGroup, model, time) %>%
      dplyr::summarise(
        n    = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        sd   = stats::sd(value, na.rm = TRUE),
        se   = sd / sqrt(n),
        ci   = se * stats::qt(1 - (1 - ci_level) / 2, df = pmax(n - 1, 1)),
        lower = mean - ci,
        upper = mean + ci,
        .groups = "drop"
      )
  }

  # --- base plot: individual trajectories (more see-through) ---------------
  plt <- df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x       = time,
        y       = value,
        group   = interaction(.data$ID, model),
        colour  = .data$baseGroup,
        linetype = model
      )
    ) +
    ggplot2::geom_line(alpha = 0.20) +  # faint individual lines
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$trajGroup),
      alpha = 0.35
    ) +
    ggplot2::scale_linetype_manual(
      values = c(observed = "solid", expected = "dotted")
    ) +
    ggplot2::labs(
      x = "Time",
      y = "Score",
      colour   = "Base group",
      shape    = "Trajectory group",
      linetype = "Model"
    ) +
    ggplot2::theme_minimal()

  # --- add group means -----------------------------------------------------
  if (show_means) {
    plt <- plt +
      ggplot2::geom_line(
        data = summary_df,
        inherit.aes = FALSE,
        ggplot2::aes(
          x      = time,
          y      = mean,
          group  = interaction(baseGroup, trajGroup, model),
          colour = baseGroup,
          linetype = model
        ),
        size  = 1.1,
        alpha = 0.9
      ) +
      ggplot2::geom_point(
        data = summary_df,
        inherit.aes = FALSE,
        ggplot2::aes(
          x      = time,
          y      = mean,
          colour = baseGroup,
          shape  = trajGroup
        ),
        size  = 2.5,
        alpha = 0.95
      )
  }

  # --- add confidence bands around means ----------------------------------
  if (show_ci) {
    plt <- plt +
      ggplot2::geom_errorbar(
        data = summary_df,
        inherit.aes = FALSE,
        ggplot2::aes(
          x      = time,
          ymin   = lower,
          ymax   = upper,
          group  = interaction(baseGroup, trajGroup, model),
          colour = baseGroup
        ),
        width = 0.1,
        alpha = 0.8
      )
  }

  plt <- plt +
    ggplot2::scale_colour_manual(values = pal$colours) +
    ggplot2::scale_shape_manual(values = pal$shapes[seq_len(nlevels(df$trajGroup))]) +
    ggplot2::scale_linetype_manual(values = pal$linetypes)

  # --- optional faceting ---------------------------------------------------
  if (isTRUE(facet)) {
    plt <- plt + ggplot2::facet_grid(baseGroup ~ trajGroup)
  }

  plt
}
