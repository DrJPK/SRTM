#' Wrap a ggplot title safely
#' @keywords internal
srtm_wrap_title <- function(p, width = 30, size = 10) {
  ttl <- p$labels$title %||% ""
  ttl_wrapped <- stringr::str_wrap(ttl, width = width)

  p +
    ggplot2::labs(title = ttl_wrapped) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size   = size,
        lineheight = 1.1,
        hjust = 0.5
      )
    )
}

#' Add baseline/trajectory group labels to a cut-point plot
#'
#' @description
#' Adds a small label inside each region of a cut-point density plot.
#' Works on the output of `plotCutPoints()`. Labels are placed at y=0
#' with a white background for readability.
#'
#' @keywords internal
srtm_add_group_overlay <- function(p,
                                   df,
                                   group_var,
                                   x_field = "x",
                                   palette = "simple",
                                   size = 4,
                                   vjust = -0.5) {

  if (!group_var %in% names(df)) {
    return(p)  # silently skip if grouping column doesn’t exist
  }

  # compute group means to position labels horizontally
  grp_tbl <- df %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      x = mean(.data[[x_field]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data[[group_var]]))

  if (nrow(grp_tbl) == 0L) {
    return(p)
  }

  grp_tbl$y <- 0  # place labels at the x-axis

  p +
    ggplot2::geom_label(
      data = grp_tbl,
      ggplot2::aes(
        x     = x,
        y     = y,
        label = .data[[group_var]]
      ),
      fill  = "white",
      colour = "black",
      size   = size,
      vjust  = vjust,
      label.padding = ggplot2::unit(0.15, "lines"),
      label.size = 0.3
    )
}
