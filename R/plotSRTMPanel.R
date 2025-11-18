#' Panel plot of SRTM trajectories for a subset of data
#'
#' @description
#' Create a panel plot of individual trajectories and expected post scores
#' from an SRTM analysis. The function is designed to work with the `data`
#' element of an object returned by [SRTMAnalyse()], and will:
#'
#' * draw individual observed trajectories from `y0`, `y1`, and `y2`;
#' * optionally overlay expected post scores (`exp_y2`) as a separate
#'   "expected" trajectory;
#' * colour and shape trajectories by the interaction of baseline group and
#'   trajectory grouping (e.g., `baseGroup` × `trajType`);
#' * facet rows by baseline level and columns by trajectory direction
#'   (if `facet = TRUE`).
#'
#' Time can be displayed either as discrete measurement occasions
#' (`"Historical"`, `"Pre"`, `"Post"`) or as a continuous scale representing
#' time since baseline (using `dt01` and `dt12`, and the stored time-unit
#' label from `.srtm_time_state` when available).
#'
#' @param data A data frame, typically the `data` component of an
#'   [SRTMAnalyse()] result. Must contain at least the columns `y0`, `y1`,
#'   `y2`, and an ID column (see `id_col`). If present, `exp_y2` is used to
#'   draw expected post trajectories. Grouping variables `baseGroup`,
#'   `trajGroup`, and/or `trajType` are used for colouring, shaping, and
#'   faceting. The columns `dt01` and `dt12` (time between `y0`–`y1` and
#'   `y1`–`y2`) are used when plotting time as a continuous scale.
#' @param id_col A string giving the name of the ID column in `data`.
#'   Defaults to `"ID"`.
#' @param facet Logical. If `TRUE` (default), the plot is faceted in a grid
#'   with rows corresponding to baseline groups and columns corresponding to
#'   trajectory groupings. If `FALSE`, all trajectories are drawn in a single
#'   panel.
#' @param palette Character string giving the palette name passed to
#'   [setPlotPalette()]. One of `"simple"`, `"pastel"`, `"modern"`,
#'   `"colourblind"`, or `"greys"`. The palette is combined across baseline
#'   and trajectory groups using [srtm_setCombinedPalette()] so that
#'   baseline levels share consistent colours and trajectory directions share
#'   consistent shapes.
#' @param plot_time_discrete Logical. If `TRUE`, time is plotted as a
#'   discrete factor with labels `"Historical"` (for `y0`), `"Pre"` (for `y1`),
#'   and `"Post"` (for `y2`). If `FALSE` (default), time is plotted as a
#'   continuous numeric scale (`t`) representing time since baseline, with
#'   breaks chosen adaptively and the x-axis label taken from the internal
#'   `.srtm_time_state$unit_label` when available (falling back to
#'   `"Time units"`).
#'
#' @details
#' The function first reshapes the data into long format, creating an
#' indicator `model` to distinguish observed and expected values:
#'
#' * Observed values: `y0`, `y1`, `y2`, with `model = "observed"`.
#' * Expected values: `y1` and `exp_y2` recoded to `time = "y2"`,
#'   with `model = "expected"`, so that expected trajectories are aligned
#'   with the observed post time point.
#'
#' A combined grouping key `group_key = interaction(baseGroup, traj_key)`
#' is used to drive both colour and shape aesthetics. Here `traj_key` is
#' taken from `trajType` if present, otherwise from `trajGroup`. Baseline
#' and trajectory labels are kept in `baseGroup_lab` and `traj_key_lab` for
#' use in facet labels.
#'
#' When `facet = TRUE`, the plot uses [ggplot2::facet_grid()] with:
#'
#' * rows = baseline groups (labelled as `"Baseline Level\n<level>"`);
#' * columns = trajectory groupings (labelled as `"Trajectory Direction\n<level>"`);
#'
#' and places facet strips on the outside of the plot for clearer panel
#' interpretation.
#'
#' @return
#' A [ggplot2::ggplot] object that can be further modified with additional
#' ggplot layers or themes if desired.
#'
#' @examples
#' # Suppose `res` is the result of SRTMAnalyse()
#' res <- SRTMAnalyse(SRTM_synth_data, interactive = FALSE, time01 = 1, time12 = 1)
#'
#' # Basic panel plot with discrete time
#' plotSRTMPanel(res$data, plot_time_discrete = TRUE)
#'
#' # Continuous-time plot using stored time units and faceting
#' plotSRTMPanel(res$data, plot_time_discrete = FALSE, palette = "colourblind")
#'
#' # Single-panel plot (no facets)
#' plotSRTMPanel(res$data, facet = FALSE)

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
        ggplot2::aes(colour = group_key),
        alpha     = 0.3,
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        data = dplyr::filter(df_long, model == "expected"),
        ggplot2::aes(colour = group_key),
        alpha     = 0.3,
        linewidth = 0.6,
        linetype  = "dotted"
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
          breaks       = breaks,
          minor_breaks = NULL
        )
    }

    # pull unit label from internal time state (with a safe fallback)
    unit_label <- tryCatch(
      .srtm_time_state$unit_label,
      error = function(...) NULL
    )
    if (is.null(unit_label) || !nzchar(unit_label)) {
      unit_label <- "Time units"
    }

    # you can tweak wording here if you prefer "Pre" instead of "baseline"
    axis_title <- glue::glue("Time since baseline ({unit_label})")

    p <- p +
      ggplot2::labs(
        x = axis_title,
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
