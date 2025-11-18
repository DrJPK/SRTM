#' Compute semantic trajectory types from slope groups
#'
#' @keywords internal
srtm_compute_trajType <- function(data,
                                  slope_col   = "m01",
                                  group_col   = "trajGroup",
                                  label_scheme = c("arrows", "signs", "text"),
                                  dt01        = NULL,
                                  steep_const = 0.25,
                                  flat_const  = 0.05) {

  label_scheme <- rlang::arg_match(label_scheme)

  if (!all(c(slope_col, group_col) %in% names(data))) {
    return(NULL)
  }

  x  <- data[[slope_col]]
  gg <- data[[group_col]]

  if (!is.numeric(x) || !is.factor(gg)) {
    return(NULL)
  }

  # determine dt01
  if (is.null(dt01)) {
    if ("dt01" %in% names(data)) {
      dt01_vec <- data$dt01
      dt01 <- stats::median(dt01_vec[is.finite(dt01_vec)], na.rm = TRUE)
    } else {
      dt01 <- 1
    }
  }

  # scale_range from y0,y1,y2 if present
  scale_cols <- intersect(c("y0", "y1", "y2"), names(data))
  if (length(scale_cols) > 0L) {
    vals <- unlist(data[scale_cols], use.names = FALSE)
    scale_range <- diff(range(vals, na.rm = TRUE))
  } else {
    scale_range <- diff(range(x * dt01, na.rm = TRUE))
  }

  if (!is.finite(scale_range) || scale_range <= 0 ||
      !is.finite(dt01)        || dt01 <= 0) {
    return(NULL)
  }

  schemes <- srtm_trajectory_label_schemes()

  thr_tbl <- tibble::tibble(
    slope_thr  = c(
      steep_const * scale_range / dt01,   # steep_pos
      flat_const  * scale_range / dt01,   # shallow_pos
      -flat_const * scale_range / dt01,   # flat
      -steep_const * scale_range / dt01,  # shallow_neg
      -Inf                                # steep_neg
    ),
    schemes
  )

  # mean slope per trajGroup
  means <- tapply(x, gg, mean, na.rm = TRUE)

  classify_one <- function(m) {
    if (!is.finite(m)) {
      return("flat")
    }
    idx <- which.max(m >= thr_tbl$slope_thr)
    as.character(thr_tbl$rank_label[idx])
  }

  rank_by_group <- vapply(means, classify_one, character(1))

  # choose label set
  lab_col <- dplyr::case_match(
    label_scheme,
    "arrows" ~ "arrow_label",
    "signs"  ~ "sign_label",
    "text"   ~ "text_label"
  )

  lab_map <- thr_tbl[[lab_col]]
  names(lab_map) <- as.character(thr_tbl$rank_label)

  # group -> semantic label
  sem_by_group <- lab_map[rank_by_group]

  # construct trajType column aligned with data rows
  trajType <- sem_by_group[as.character(gg)]

  # factor ordering: semantic order + "Unknown" last
  if (label_scheme == "arrows") {
    desired <- c("↟", "↑", "→", "↓", "↡", "Unknown")
  } else if (label_scheme == "signs") {
    desired <- c("++", "+", "—", "-", "--", "Unknown")
  } else {
    desired <- c("steep_pos", "shallow_pos", "flat", "shallow_neg", "steep_neg", "Unknown")
  }

  desired <- c(srtm_trajectory_label_schemes()%>%dplyr::pull(lab_col),"Unknown")

  present <- intersect(desired, unique(trajType))
  trajType <- factor(trajType, levels = present, ordered = TRUE)

  attr(trajType, "srtm_slope_thresholds") <- list(
    slope_col   = slope_col,
    group_col   = group_col,
    dt01        = dt01,
    scale_range = scale_range,
    steep_const = steep_const,
    flat_const  = flat_const,
    thresholds  = thr_tbl
  )

  trajType
}
