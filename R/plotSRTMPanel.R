#' Panel plot of SRTM trajectories for a subset of data
#'
#' @param data A data frame like the `data` element of an `srtm_analysis`
#'   object, containing at least `y0`, `y1`, `y2`, and optionally `exp_y2`,
#'   `baseGroup`, `trajGroup`, and `trajType`.
#' @param id_col Name of the ID column. Defaults to `"ID"`.
#' @param facet Logical. If `TRUE` (default), use facets for trajectories
#'   (trajType if present, otherwise trajGroup).
#' @param palette Character palette name passed to [setPlotPalette()].
#'
#' @return A ggplot object.
#'
#' @export
plotSRTMPanel <- function(data,
                          id_col  = "ID",
                          facet   = TRUE,
                          palette = c("simple", "pastel", "modern", "colourblind", "greys")) {

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
      dplyr::select(dplyr::all_of(c(id_col, "baseGroup", traj_facet, "exp_y2"))) %>%
      dplyr::rename(value = "exp_y2") %>%
      dplyr::mutate(
        time  = "y2",
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
    )%>%
    dplyr::mutate(
      t = dplyr::case_when(
        time == "y0" ~ -dt01,
        time == "y1" ~ 0,
        time == "y2" ~ dt12,
        TRUE         ~ NA
      )
    )

  # -------------------------------------------------------------------------
  # Combined legend key: interaction(baseGroup, trajType/trajGroup)
  # -------------------------------------------------------------------------
  df_long <- df_long %>%
    dplyr::mutate(
      baseGroup = as.factor(.data$baseGroup),
      traj_key  = .data[[traj_facet]],
      traj_key  = as.factor(traj_key),
      group_key = interaction(
        baseGroup,
        traj_key,
        drop = TRUE,
        sep  = ""
      )
    )

  # Build combined palette for group_key
  comb_levels <- levels(df_long$group_key)
  comb_pal    <- srtm_setCombinedPalette(comb_levels, palette = palette)

  # -------------------------------------------------------------------------
  # Plot
  # -------------------------------------------------------------------------
  id_sym <- rlang::sym(id_col)

  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x     = time,
      y     = value,
      group = !!id_sym
    )
  ) +
    # neutral per-ID trajectories
    ggplot2::geom_line(
      colour   = "grey70",
      alpha    = 0.3,
      linewidth = 0.4
    ) +
    # points coloured and shaped by combined group
    ggplot2::geom_point(
      ggplot2::aes(
        colour = group_key,
        shape  = group_key
      ),
      alpha = 0.7,
      size  = 2
    ) +
    ggplot2::scale_colour_manual(
      values = comb_pal$colours,
      name   = "Baseline × Trajectory"
    ) +
    ggplot2::scale_shape_manual(
      values = comb_pal$shapes,
      name   = "Baseline × Trajectory"
    ) +
    ggplot2::labs(
      x = "Time point",
      y = "Score"
    ) +
    ggplot2::theme_minimal()

  if (facet) {
    # facet rows by baseGroup, cols by traj facet (trajType or trajGroup)
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(baseGroup),
        cols = ggplot2::vars(traj_key),
        scales = "free_y"
      )
  }

  p
}
