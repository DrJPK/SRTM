#' Panel plot of SRTM trajectories for a subset of data
#'
#' @param data A data frame like the `data` element of an `srtm_analysis`
#'   object, containing at least `y0`, `y1`, `y2`, and optionally `exp_y2`,
#'   `baseGroup`, `trajGroup`, and `trajType`, plus `dt01`, `dt12`.
#' @param id_col Name of the ID column. Defaults to `"ID"`.
#' @param facet Logical. If `TRUE` (default), use facets for trajectories
#'   (trajType if present, otherwise trajGroup).
#' @param palette Character palette name passed to [setPlotPalette()].
#' @param plot_time_discrete Logical. If `TRUE`, plot time as a discrete
#'   factor with levels labelled `"Historical"`, `"Pre"`, and `"Post"`.
#'   If `FALSE` (default), plot time as a continuous numeric scale based on
#'   the `t` variable (time since baseline), with breaks spaced at least 1 unit
#'   apart.
#'
#' @return A ggplot object.
#'
#' @export
plotSRTMPanel <- function(data,
                          id_col            = "ID",
                          facet             = TRUE,
                          palette           = c("simple", "pastel", "modern", "colourblind", "greys"),
                          plot_time_discrete = FALSE) {

  palette <- rlang::arg_match(palette)
  df      <- tibble::as_tibble(data)

  if (!id_col %in% names(df)) {
    rlang::abort(
      glue::glue("Column `{id_col}` not found in `data`."),
      class = "srtm_plot_bad_id_col"
    )
  }

  # choose trajectory facet/group variable
  traj_facet <- dplyr::case_when(
    "trajType"  %in% names(df) ~ "trajType",
    "trajGroup" %in% names(df) ~ "trajGroup",
    TRUE                       ~ NA_character_
  )

  if (is.na(traj_facet)) {
    rlang::abort(
      "Neither `trajType` nor `trajGroup` found in `data`.",
      class = "srtm_plot_no_traj"
    )
  }

  # ensure baseGroup exists (or create a dummy one)
  if (!"baseGroup" %in% names(df)) {
    df$baseGroup <- factor("All", levels = "All")
  }

  # -------------------------------------------------------------------------
  # Build long data: observed and expected
  # -------------------------------------------------------------------------
  has_exp <- "exp_y2" %in% names(df)

  # observed: y0, y1, y2
  obs_long <- df %>%
    tidyr::pivot_longer(
      cols      = tidyselect::all_of(c("y0", "y1", "y2")),
      names_to  = "time",
      values_to = "value"
    ) %>%
    dplyr::mutate(model = "observed")

  if (has_exp) {
    exp_long <- df %>%
      dplyr::select(
        dplyr::all_of(c(id_col, "baseGroup", traj_facet, "y1", "exp_y2", "dt01", "dt12"))
      ) %>%
      tidyr::pivot_longer(
        cols      = c("y1", "exp_y2"),
        names_to  = "time",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        # rename exp_y2 -> y2 for plotting
        time  = dplyr::recode(time, "exp_y2" = "y2"),
        model = "expected"
      )

    df_long <- dplyr::bind_rows(obs_long, exp_long)
  } else {
    df_long <- obs_long
  }

  df_long <- df_long %>%
    dplyr::mutate(
      time  = factor(time, levels = c("y0", "y1", "y2")),
      model = factor(model, levels = unique(model))
    ) %>%
    dplyr::mutate(
      t = dplyr::case_when(
        time == "y0" ~ -dt01,
        time == "y1" ~ 0,
        time == "y2" ~ dt12,
        TRUE         ~ NA_real_
      )
    )

  # -------------------------------------------------------------------------
  # Combined legend key: interaction(baseGroup, trajType/trajGroup)
  # -------------------------------------------------------------------------
  df_long <- df_long %>%
    dplyr::mutate(
      baseGroup = droplevels(as.factor(.data$baseGroup)),
      traj_key  = droplevels(as.factor(.data[[traj_facet]])),
      baseGroup_lab = baseGroup,
      traj_key_lab  = traj_key,
      group_key = interaction(
        baseGroup,
        traj_key,
        drop = TRUE,
        sep  = ""
      )
    )

  # Build combined palette for group_key
  base_levels <- levels(df_long$baseGroup)
  traj_levels <- levels(df_long$traj_key)

  comb_pal <- srtm_setCombinedPalette(
    levels_c = base_levels,
    levels_s = traj_levels,
    palette  = palette
  )

  id_sym <- rlang::sym(id_col)

  # -------------------------------------------------------------------------
  # Base plot: branch on discrete vs continuous time
  # -------------------------------------------------------------------------
  if (isTRUE(plot_time_discrete)) {
    # x as discrete factor (y0/y1/y2 -> Historical/Pre/Post)
    p <- ggplot2::ggplot(
      data = dplyr::filter(df_long, model == "observed"),
      ggplot2::aes(
        x     = time,
        y     = value,
        group = !!id_sym
      )
    ) +
      ggplot2::geom_line(ggplot2::aes(colour = group_key, linetype = model),
        alpha     = 0.3,
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        data = dplyr::filter(df_long, model == "expected"),
        ggplot2::aes(colour = group_key,
                     linetype = model),
        alpha     = 0.3,
        linewidth = 0.6
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = group_key,
          shape  = group_key
        ),
        alpha = 0.7,
        size  = 2
      ) +
      ggplot2::scale_x_discrete(
        labels = c(
          y0 = "Historical",
          y1 = "Pre",
          y2 = "Post"
        )
      ) +
      ggplot2::labs(
        x = "Time point",
        y = "Score"
      )

  } else {
    # x as continuous time since baseline (t)
    p <- ggplot2::ggplot(
      data = dplyr::filter(df_long, model == "observed"),
      ggplot2::aes(
        x     = t,
        y     = value,
        group = !!id_sym
      )
    ) +
      ggplot2::geom_line(
        ggplot2::aes(colour = group_key, linetype = model),
        alpha     = 0.3,
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        data = dplyr::filter(df_long, model == "expected"),
        ggplot2::aes(colour = group_key,
                     linetype = model),
        alpha     = 0.3,
        linewidth = 0.6
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = group_key,
          shape  = group_key
        ),
        alpha = 0.7,
        size  = 2
      )

    # sensible breaks with spacing >= 1
    t_range <- range(df_long$t, na.rm = TRUE)
    if (all(is.finite(t_range)) && diff(t_range) > 0) {
      approx_step <- diff(t_range) / 3
      step        <- max(1, ceiling(approx_step))
      breaks      <- seq(
        from = floor(t_range[1]),
        to   = ceiling(t_range[2]),
        by   = step
      )
      # always include 0 if in range
      if (t_range[1] <= 0 && t_range[2] >= 0) {
        breaks <- sort(unique(c(breaks, 0)))
      }

      p <- p +
        ggplot2::scale_x_continuous(
          breaks = breaks,
          minor_breaks = NULL
        )
    }

    p <- p +
      ggplot2::labs(
        x = "Time since baseline measurement (same units as dt01/dt12)",
        y = "Score"
      )
  }

  # shared colour/shape scales and theme
  p <- p +
    ggplot2::scale_colour_manual(
      values = comb_pal$colours,
      name   = "Baseline × Trajectory"
    ) +
    ggplot2::scale_shape_manual(
      values = comb_pal$shapes,
      name   = "Baseline × Trajectory"
    ) +
    ggplot2::theme_minimal()

  if (facet) {
    p <- p +
      ggplot2::facet_grid(
        rows   = ggplot2::vars(baseGroup_lab),
        cols   = ggplot2::vars(traj_key_lab),
        scales = "free_y",
        switch = "both",
        labeller = ggplot2::labeller(
          baseGroup_lab = function(x) paste0("Baseline Level\n", x),
          traj_key_lab  = function(x) paste0("Trajectory Direction\n", x)
        )
      ) +
      ggplot2::theme(
        strip.placement     = "outside",
        strip.background    = ggplot2::element_rect(fill = "grey95"),
        strip.text.x        = ggplot2::element_text(face = "bold"),
        strip.text.y.right  = ggplot2::element_text(angle = 90, face = "bold")
      )
  }

  p
}
