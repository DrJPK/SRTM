#' Compute semantic trajectory types from slopes and groups
#'
#' @description
#' Internal helper used by [SRTMAnalyse()] to convert raw trajectory
#' slopes (e.g., `m01`) and grouping factors (e.g., `trajGroup`) into
#' semantically meaningful trajectory types such as *steep positive*,
#' *shallow negative*, or *flat*. The function classifies each
#' trajectory group based on its mean slope and returns a factor
#' `trajType` aligned with the rows of `data`.
#'
#' @param data A data frame containing at least:
#'   * a numeric slope column (e.g. `m01`), and
#'   * a factor grouping column (e.g. `trajGroup`).
#'   If available, the columns `y0`, `y1`, and/or `y2` are used to
#'   estimate the outcome scale range, and `dt01` is used (or estimated)
#'   as the time difference between `y0` and `y1`.
#' @param slope_col Name of the numeric column in `data` containing
#'   the slope for the first time interval (typically `m01`). Can be a
#'   string; defaults to `"m01"`.
#' @param group_col Name of the factor column in `data` that indexes
#'   trajectory groups (typically `trajGroup`). Can be a string;
#'   defaults to `"trajGroup"`.
#' @param label_scheme Character string specifying how the trajectory
#'   types should be labelled. One of:
#'   * `"arrows"` — unicode arrows (↟, ↑, →, ↓, ↡),
#'   * `"signs"`  — sign-like labels (`"++"`, `"+"`, `"—"`, `"-"`, `"--"`),
#'   * `"text"`   — textual labels (`"steep_pos"`, `"shallow_pos"`,
#'     `"flat"`, `"shallow_neg"`, `"steep_neg"`).
#' @param dt01 Optional numeric giving the time interval between the first
#'   and second measurement occasions (e.g., between `y0` and `y1`).
#'   If `NULL`, the function will:
#'   * use the median of `data$dt01` when present, or
#'   * default to `1` otherwise.
#' @param steep_const Numeric constant (default `0.25`) controlling the
#'   threshold for defining *steep* vs *shallow* trajectories. The
#'   threshold in slope units is calculated as
#'   `steep_const * scale_range / dt01`, where `scale_range` is the
#'   approximate range of the outcome scale.
#' @param flat_const Numeric constant (default `0.05`) controlling the
#'   threshold for defining *flat* trajectories around zero change. The
#'   band in slope units is calculated as
#'   `± flat_const * scale_range / dt01`.
#'
#' @details
#' The function proceeds in several steps:
#'
#' \enumerate{
#'   \item It validates that `data[[slope_col]]` is numeric and
#'     `data[[group_col]]` is a factor. If not, `NULL` is returned.
#'   \item The effective time interval `dt01` is determined from the
#'     argument or from `data$dt01` (median of finite values), defaulting
#'     to `1` if missing.
#'   \item The outcome scale range `scale_range` is estimated:
#'     \itemize{
#'       \item from the combined range of `y0`, `y1`, and `y2` when
#'         available, or
#'       \item from the range of `slope * dt01` otherwise.
#'     }
#'   \item Five ordered thresholds in slope units are constructed:
#'     \itemize{
#'       \item `+steep_const * scale_range / dt01` (steep positive),
#'       \item `+flat_const  * scale_range / dt01` (shallow positive),
#'       \item `-flat_const  * scale_range / dt01` (flat),
#'       \item `-steep_const * scale_range / dt01` (shallow negative),
#'       \item `-Inf` (steep negative).
#'     }
#'   \item For each trajectory group in `group_col`, the mean slope is
#'     computed and classified into one of the five rank levels
#'     (steep_pos, shallow_pos, flat, shallow_neg, steep_neg) based on
#'     these thresholds.
#'   \item Depending on `label_scheme`, the rank levels are mapped to
#'     arrow, sign, or text labels using
#'     [srtm_trajectory_label_schemes()].
#'   \item A `trajType` factor is created for each row of `data` by
#'     aligning the semantic label of its group. The factor levels are
#'     ordered according to the semantic trajectory order plus
#'     `"Unknown"` last, and unused levels are dropped.
#' }
#'
#' The function attaches an attribute `"srtm_slope_thresholds"` to the
#' returned factor, containing:
#'
#' * `slope_col`, `group_col`, `dt01`, `scale_range`,
#' * `steep_const`, `flat_const`,
#' * `thresholds` — the internal tibble of thresholds and labels used.
#'
#' This attribute is used by [SRTMAnalyse()] to store slope-threshold
#' settings in the `settings$trajThresholds` component for later
#' inspection or reporting.
#'
#' If any critical inputs are missing or invalid (e.g., non-positive
#' `scale_range` or `dt01`), the function returns `NULL`.
#'
#' @return
#' An ordered factor vector `trajType` of length `nrow(data)` whose
#' levels are semantic trajectory labels determined by `label_scheme`
#' (plus `"Unknown"` when present), with an attribute
#' `"srtm_slope_thresholds"` describing the thresholds used; or `NULL`
#' if the inputs are insufficient to compute trajectory types.
#'
#' @seealso
#' [SRTMAnalyse()], [plotSRTMPanel()], [srtm_trajectory_label_schemes()]
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
