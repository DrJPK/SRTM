#----
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

#----
#' Summary panel plot of SRTM trajectories (box/violin by time)
#'
#' @description
#' Creates a facetted panel plot showing the distribution of scores at each
#' time point (Historical, Pre, Post) for each Baseline × Trajectory cell in
#' an SRTM analysis, using either boxplots or violins. Observed scores are
#' filled with the usual combined SRTM palette colour, and expected post
#' scores (exp_y2) are shown as white-filled, coloured outlines, dodged
#' horizontally at the Post time point.
#'
#' @param data A data frame, typically the `data` component of an
#'   [SRTMAnalyse()] result. Must contain `y0`, `y1`, `y2`, `baseGroup`,
#'   and either `trajType` or `trajGroup`. If `exp_y2` is present it will
#'   be used as expected Post scores.
#' @param id_col Name of the ID column (unused for aggregation, but kept
#'   for consistency with [plotSRTMPanel()]). Default `"ID"`.
#' @param facet Logical; if `TRUE` (default) facet rows by baseline and
#'   columns by trajectory direction.
#' @param palette Palette name passed to [setPlotPalette()].
#'   One of `"simple"`, `"pastel"`, `"modern"`, `"colourblind"`, `"greys"`.
#' @param geom Character; `"boxplot"` (default) or `"violin"`.
#' @param width Numeric width of each box/violin (passed to the geom).
#' @param dodge_width Numeric total dodge width between observed and
#'   expected at Post.
#' @param expected_fill Fill colour used for expected distributions
#'   (default `"white"`).
#' @param show_legend Logical; if `TRUE` (default), a simple custom legend
#'   is added below the panel indicating which glyph corresponds to
#'   observed vs expected scores (grey square with black border for
#'   observed; white square with grey border for expected).
#'
#' @return A [ggplot2::ggplot] object. If `show_legend = TRUE`, this is a
#'   patchwork object combining the main panel and a small legend row.
#'
#' @export
plotSRTMPanelSummary <- function(data,
                                 id_col        = "ID",
                                 facet         = TRUE,
                                 palette       = c("simple", "pastel", "modern", "colourblind", "greys"),
                                 geom          = c("boxplot", "violin"),
                                 width         = 0.35,
                                 dodge_width   = 0.5,
                                 expected_fill = "white",
                                 show_legend   = TRUE) {

  palette <- rlang::arg_match(palette)
  geom    <- rlang::arg_match(geom)

  df <- tibble::as_tibble(data)

  if (!id_col %in% names(df)) {
    rlang::abort(
      glue::glue("Column `{id_col}` not found in `data`."),
      class = "srtm_plotsummary_bad_id_col"
    )
  }

  # choose trajectory facet/group variable (trajType preferred)
  traj_facet <- dplyr::case_when(
    "trajType"  %in% names(df) ~ "trajType",
    "trajGroup" %in% names(df) ~ "trajGroup",
    TRUE                       ~ NA_character_
  )

  if (is.na(traj_facet)) {
    rlang::abort(
      "Neither `trajType` nor `trajGroup` found in `data`.",
      class = "srtm_plotsummary_no_traj"
    )
  }

  # ensure baseGroup exists (or create a dummy one)
  if (!"baseGroup" %in% names(df)) {
    df$baseGroup <- factor("All", levels = "All")
  }

  has_exp <- "exp_y2" %in% names(df)

  # --------------------------------------------------------------------------
  # Build long data: observed (y0,y1,y2) and, if present, expected y2
  # --------------------------------------------------------------------------
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
        dplyr::all_of(c(id_col, "baseGroup", traj_facet, "exp_y2"))
      ) %>%
      dplyr::mutate(
        time   = "y2",
        value  = .data$exp_y2,
        model  = "expected"
      ) %>%
      dplyr::select(-.data$exp_y2)

    df_long <- dplyr::bind_rows(obs_long, exp_long)
  } else {
    df_long <- obs_long
  }

  # Discrete time only: Historical / Pre / Post
  df_long <- df_long %>%
    dplyr::mutate(
      time         = factor(time, levels = c("y0", "y1", "y2")),
      time_label   = dplyr::recode(
        time,
        y0 = "Historical",
        y1 = "Pre",
        y2 = "Post"
      ),
      baseGroup     = droplevels(as.factor(.data$baseGroup)),
      traj_key      = droplevels(as.factor(.data[[traj_facet]])),
      baseGroup_lab = baseGroup,
      traj_key_lab  = traj_key,
      group_key     = interaction(
        baseGroup,
        traj_key,
        drop = TRUE,
        sep  = ""
      ),
      model         = factor(model, levels = c("observed", "expected"))
    )

  # --------------------------------------------------------------------------
  # Combined palette for Baseline × Trajectory (used for observed fills)
  # --------------------------------------------------------------------------
  base_levels <- levels(df_long$baseGroup)
  traj_levels <- levels(df_long$traj_key)

  comb_pal <- srtm_setCombinedPalette(
    levels_c = base_levels,
    levels_s = traj_levels,
    palette  = palette
  )

  group_levels <- levels(df_long$group_key)
  cols <- comb_pal$colours

  cols <- cols[names(cols) %in% group_levels]

  if (length(cols) == 0L) {
    rlang::warn(
      "plotSRTMPanelSummary(): no Baseline × Trajectory combinations matched the combined palette names.",
      class = "srtm_plotsummary_palette_mismatch"
    )
  } else {
    cols <- cols[match(group_levels, names(cols))]
    names(cols) <- group_levels
  }

  # --------------------------------------------------------------------------
  # 3-way interaction for fills: Baseline × Trajectory × Model
  # --------------------------------------------------------------------------
  df_long <- df_long %>%
    dplyr::mutate(
      fill_key = interaction(group_key, model, drop = TRUE, sep = "::")
    )

  fill_levels <- levels(df_long$fill_key)

  # default all fills to expected_fill
  fill_vals <- stats::setNames(
    rep(expected_fill, length(fill_levels)),
    fill_levels
  )

  # overwrite observed fills with group colours where available
  for (g in group_levels) {
    obs_key <- paste0(g, "::observed")
    exp_key <- paste0(g, "::expected")

    if (obs_key %in% fill_levels && g %in% names(cols)) {
      fill_vals[obs_key] <- cols[[g]]
    }
    if (exp_key %in% fill_levels) {
      fill_vals[exp_key] <- expected_fill
    }
  }

  # --------------------------------------------------------------------------
  # Single geom with dodging by model; black whiskers/edges, coloured fills
  # --------------------------------------------------------------------------
  pos_dodge <- ggplot2::position_dodge2(width = dodge_width, preserve = "single")

  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x     = time_label,
      y     = value,
      group = interaction(time_label, group_key, model),
      fill  = fill_key
    )
  )

  if (geom == "boxplot") {

    p <- p +
      ggplot2::geom_boxplot(
        colour   = "black",      # black whiskers + frame
        width    = width,
        position = pos_dodge,
        alpha    = 0.9
      )

  } else {  # geom == "violin"

    p <- p +
      ggplot2::geom_violin(
        colour   = "black",      # black outline
        trim     = FALSE,
        width    = width,
        position = ggplot2::position_dodge(width = dodge_width),
        alpha    = 0.9
      )

  }

  p <- p +
    ggplot2::scale_fill_manual(
      values = fill_vals,
      guide  = "none"  # no Baseline × Trajectory legend; facets carry that info
    ) +
    ggplot2::labs(
      x = "Time point",
      y = "Score"
    ) +
    ggplot2::theme_minimal()

  # --------------------------------------------------------------------------
  # Facetting (same as plotSRTMPanel)
  # --------------------------------------------------------------------------
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
        strip.placement    = "outside",
        strip.background   = ggplot2::element_rect(fill = "grey95"),
        strip.text.x       = ggplot2::element_text(face = "bold"),
        strip.text.y.right = ggplot2::element_text(angle = 90, face = "bold")
      )
  }

  # --------------------------------------------------------------------------
  # Optional custom legend for observed vs expected
  # --------------------------------------------------------------------------
  if (!isTRUE(show_legend)) {
    return(p)
  }

  legend_df <- tibble::tibble(
    x      = 0.5,
    y      = c(2, 1.3),    # stacked vertically: Observed above Expected
    label  = c("Observed", "Expected"),
    fill   = c("grey60", expected_fill),
    border = c("black", "grey40")
  )

  legend_title_df <- tibble::tibble(
    x = 1,
    y = 2.4,
    label = "Data Type"
  )

  legend_plot <- ggplot2::ggplot() +

    ggplot2::geom_text(
      data = legend_title_df,
      ggplot2::aes(x = x, y = y, label = label),
      fontface = "bold",
      size = 3.3,
      hjust = 0.5,
      vjust = 0
    ) +
    # squares
    ggplot2::geom_tile(
      data = legend_df,
      ggplot2::aes(x = x, y = y, fill = label),
      width  = 0.35,
      height = 0.35,
      colour = legend_df$border,
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    # manual fill, consistent with tiles
    ggplot2::scale_fill_manual(
      values = c(
        "Observed" = "grey60",
        "Expected" = expected_fill
      ),
      guide = "none"
    ) +
    # labels to the right of tiles
    ggplot2::geom_text(
      data = legend_df,
      ggplot2::aes(x = x + 0.45, y = y, label = label),
      hjust = 0,
      vjust = 0.5,
      size  = 3
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0.5, 2.6),
      ylim = c(1.0, 2.7),
      clip = "off",
      ratio = 1
    ) +
    ggplot2::theme_void()

  # Combine main plot + legend row via patchwork
  patchwork::wrap_plots(
    p,
    legend_plot,
    ncol    = 2,
    widths = c(5, 1.2)
  )
}
#----
