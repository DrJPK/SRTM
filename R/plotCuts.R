#' Plot density and cut-points from an SRTM group suggestion
#'
#' @description
#' Produces a publication-ready density plot showing the kernel density
#' estimate and the cut-points implied by an `"srtm_group_suggestion"`
#' object returned by [findGroups()]. This is useful for reporting how
#' baseline or slope-based groups were defined, and can be combined with
#' other plots using patchwork.
#'
#' @param group_params An object of class `"srtm_group_suggestion"`,
#'   typically the result of [findGroups()]. It must contain at least:
#'   \itemize{
#'     \item `density` — the main [stats::density()] object,
#'     \item `minima_x` — numeric vector of local minima locations,
#'     \item `nGroups` — final number of groups used,
#'     \item `time_var` — name of the variable used for grouping,
#'     \item `bw` — bandwidth argument passed to [stats::density()],
#'     \item `adjust` — bandwidth adjustment factor.
#'   }
#' @param x_label Optional character string for the x-axis label. If
#'   `NULL` (default), a label is chosen based on `group_params$time_var`:
#'   `"baseline score"` for `"y1"`, `"historical slope"` for `"m01"`,
#'   otherwise the raw `time_var` name.
#' @param annotate Logical. If `TRUE` (default), annotations showing the
#'   numeric x-coordinates of the cut-points are added near the top of
#'   the panel.
#' @param decimals Integer number of decimal places to show in the
#'   cut-point annotations. Default is `2`.
#' @param shade Logical. If `TRUE` (default), the density is filled with
#'   semi-transparent coloured bands corresponding to the groups implied
#'   by the cut-points. If `FALSE`, only the (unfilled) density curve and
#'   vertical cut-lines are drawn.
#' @param palette Character palette name passed to [setPlotPalette()]
#'   to ensure visual consistency with other SRTM plots. One of
#'   `"simple"`, `"pastel"`, `"modern"`, `"colourblind"`, `"greys"`.
#'
#' @details
#' This function does **not** recompute kernel densities. It uses the
#' density stored in `group_params$density` (the main density for the
#' chosen `bw` and `adjust`) and the minima stored in
#' `group_params$minima_x`.
#'
#' The cut-points that define groups are reconstructed using the same
#' logic as [assignGroups()]:
#'
#' \enumerate{
#'   \item If `nGroups == 1`, no cut-points are used and the entire
#'         distribution is treated as a single group.
#'   \item Otherwise, the number of minima must be at least `nGroups - 1`.
#'   \item If there are more minima than needed, a subset is chosen by
#'         spreading indices approximately evenly across the minima,
#'         giving `nGroups - 1` cut-points.
#' }
#'
#' When `shade = TRUE`, the area under the density curve is partitioned
#' into `nGroups` contiguous regions using these cut-points, and filled
#' using colours derived from [setPlotPalette()]. The density line itself
#' is drawn in black for clarity.
#'
#' Vertical dashed lines are drawn at each cut-point. When
#' `annotate = TRUE`, the x-coordinates of the cut-points are shown as
#' text labels near the top of the plot (formatted to `decimals`
#' decimal places).
#'
#' @return
#' A `ggplot` object showing the density, cut-points, and (optionally)
#' shaded group regions and annotated cut-point values. The plot is not
#' printed automatically, making it suitable for composition with
#' patchwork or other plotting workflows.
#'
#' @examples
#' \dontrun{
#' df <- generateSynthData(n = 300, seed = 123)
#' gp <- findGroups(df, time_var = "y1", interactive = FALSE, show_plot = FALSE)
#'
#' p <- plotCutPoints(gp, x_label = "Baseline score")
#' p
#'
#' # combine multiple cut-point plots with patchwork
#' gp_y1  <- findGroups(df, time_var = "y1", interactive = FALSE, show_plot = FALSE)
#' gp_m01 <- findGroups(df, time_var = "m01", interactive = FALSE, show_plot = FALSE)
#' p1 <- plotCutPoints(gp_y1,  x_label = "Baseline score")
#' p2 <- plotCutPoints(gp_m01, x_label = "Historical slope")
#' p1 + p2
#' }
#'
#' @export
plotCutPoints <- function(group_params,
                          title_var = NULL,
                          x_label   = NULL,
                          annotate  = TRUE,
                          decimals  = 2L,
                          shade     = TRUE,
                          palette   = c("simple", "pastel", "modern", "colourblind", "greys")) {

  palette <- rlang::arg_match(palette)

  if (is.null(group_params) || !inherits(group_params, "srtm_group_suggestion")) {
    rlang::abort(
      "`group_params` must be an object of class 'srtm_group_suggestion'.",
      class = "srtm_plotCutPoints_bad_group_params"
    )
  }

  dens_main <- group_params$density
  minima_x  <- sort(group_params$minima_x %||% numeric(0))
  time_var  <- group_params$time_var %||% "value"
  nGroups   <- group_params$nGroups %||% group_params$suggested_nGroups %||% NA_integer_

  if (is.null(dens_main) || !inherits(dens_main, "density")) {
    rlang::abort(
      "`group_params$density` must be a stats::density() object.",
      class = "srtm_plotCutPoints_no_density"
    )
  }

  # Effective title variable: use supplied title_var if given, else fall back to time_var
  effective_title_var <- if (!is.null(title_var) && nzchar(title_var)) {
    title_var
  } else {
    time_var
  }

  # Default x-label if not supplied
  if (is.null(x_label)) {
    x_label <- time_var
  }

  dens_df <- tibble::tibble(
    x = dens_main$x,
    y = dens_main$y
  )

  # --------------------------------------------------------------------------
  # Build shading regions if requested
  # --------------------------------------------------------------------------
  if (isTRUE(shade) && length(minima_x) > 0L) {
    # breaks for regions: (-Inf, cut1], (cut1, cut2], ..., (last, +Inf)
    breaks <- c(-Inf, minima_x, Inf)

    # dummy "group labels" for regions; we just need distinct values
    region_ids <- seq_len(length(breaks) - 1L)

    # assign each x in dens_df to a region by cut()
    dens_df <- dens_df %>%
      dplyr::mutate(
        region = cut(
          x,
          breaks = breaks,
          include.lowest = TRUE,
          right = TRUE,
          labels = region_ids
        )
      )

    # palette for regions – only need up to nGroups, fallback if NA
    n_reg <- length(region_ids)
    base_pal <- setPlotPalette(palette)
    region_cols <- rep(base_pal$colours, length.out = n_reg)
    names(region_cols) <- as.character(region_ids)
  } else {
    dens_df$region <- factor(1L)
    region_cols <- setPlotPalette(palette)$colours[1]
    names(region_cols) <- "1"
  }

  # y-values at minima for annotation & vertical lines
  minima_df <- tibble::tibble(
    x = minima_x,
    y = approx(dens_main$x, dens_main$y, xout = minima_x)$y
  )

  # --------------------------------------------------------------------------
  # Build plot
  # --------------------------------------------------------------------------
  p <- ggplot2::ggplot(dens_df, ggplot2::aes(x = x, y = y))

  if (isTRUE(shade)) {
    p <- p +
      ggplot2::geom_area(
        ggplot2::aes(fill = region),
        alpha = 0.5,
        colour = NA
      ) +
      ggplot2::scale_fill_manual(values = region_cols, guide = "none")
  }

  # density line in black
  p <- p +
    ggplot2::geom_line(colour = "black") +
    ggplot2::geom_vline(
      xintercept = minima_x,
      linetype   = "dashed"
    )

  if (isTRUE(annotate) && length(minima_x) > 0L) {
    lab_vals <- round(minima_x, decimals)
    p <- p +
      ggplot2::geom_point(
        data = minima_df,
        ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = minima_df,
        ggplot2::aes(
          x = x,
          y = y,
          label = lab_vals
        ),
        vjust = -0.5,
        size  = 3
      )
  }

  subtitle_text <- if (is.na(nGroups)) {
    NULL
  } else {
    glue::glue("Number of groups: {nGroups}")
  }

  p +
    ggplot2::labs(
      x        = x_label,
      y        = "Density",
      title    = glue::glue("Density of {effective_title_var} with cut-points"),
      subtitle = subtitle_text
    ) +
    ggplot2::theme_minimal()
}
