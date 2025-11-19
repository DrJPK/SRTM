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
#'   If available, the columns named in `outcome_cols` are used to
#'   estimate the outcome scale range, and `dt01` is used (or estimated)
#'   as the time difference between the first two measurement occasions.
#'
#' @param slope_col Name of the numeric column in `data` containing
#'   the slope for the first time interval (typically `m01`). Can be a
#'   string; defaults to `"m01"`.
#'
#' @param group_col Name of the factor column in `data` that indexes
#'   trajectory groups (typically `trajGroup`). Can be a string;
#'   defaults to `"trajGroup"`.
#'
#' @param label_scheme Character string specifying how the trajectory
#'   types should be labelled. One of:
#'   * `"arrows"` — unicode arrows (↟, ↑, →, ↓, ↡),
#'   * `"signs"`  — sign-like labels (`"++"`, `"+"`, `"—"`, `"-"`, `"--"`),
#'   * `"text"`   — textual labels (`"steep_pos"`, `"shallow_pos"`,
#'     `"flat"`, `"shallow_neg"`, `"steep_neg"`).
#'
#' @param dt01 Optional numeric giving the time interval between the first
#'   and second measurement occasions. If `NULL`, the function will:
#'   * use the median of `data$dt01` when present, or
#'   * default to `1` otherwise.
#'
#' @param steep_const Numeric constant (default `0.25`) controlling the
#'   threshold for defining *steep* vs *shallow* trajectories. The
#'   threshold in slope units is calculated as
#'   `steep_const * scale_range / dt01`, where `scale_range` is the
#'   approximate range of the outcome scale.
#'
#' @param flat_const Numeric constant (default `0.05`) controlling the
#'   threshold for defining *flat* trajectories around zero change. The
#'   band in slope units is calculated as
#'   `± flat_const * scale_range / dt01`.
#'
#' @param outcome_cols Optional character vector naming columns in `data`
#'   that represent the outcome scale (e.g., c("y0","y1","y2")). When
#'   provided, the combined range of these columns is used to estimate
#'   `scale_range`. If `NULL`, the function defaults to
#'   `c("y0", "y1", "y2")` and uses the intersection with `names(data)`;
#'   if none are found, it falls back to the range of `slope * dt01`.
#'
#'
#' @keywords internal
srtm_compute_trajType <- function(data,
                                  slope_col    = "m01",
                                  group_col    = "trajGroup",
                                  label_scheme = c("arrows", "signs", "text"),
                                  dt01         = NULL,
                                  steep_const  = 0.25,
                                  flat_const   = 0.05,
                                  outcome_cols = NULL) {

  label_scheme <- rlang::arg_match(label_scheme)

  if (!all(c(slope_col, group_col) %in% names(data))) {
    return(NULL)
  }

  x  <- data[[slope_col]]
  gg <- data[[group_col]]

  if (!is.numeric(x) || !is.factor(gg)) {
    return(NULL)
  }

  # ---- determine dt01 -----------------------------------------------------
  if (is.null(dt01)) {
    if ("dt01" %in% names(data)) {
      dt01_vec <- data$dt01
      dt01 <- stats::median(dt01_vec[is.finite(dt01_vec)], na.rm = TRUE)
    } else {
      dt01 <- 1
    }
  }

  # ---- estimate outcome scale range ---------------------------------------
  if (is.null(outcome_cols)) {
    # default: try y0, y1, y2 if present
    scale_cols <- intersect(c("y0", "y1", "y2"), names(data))
  } else {
    scale_cols <- intersect(outcome_cols, names(data))
  }

  if (length(scale_cols) > 0L) {
    vals <- unlist(data[scale_cols], use.names = FALSE)
    scale_range <- diff(range(vals, na.rm = TRUE))
  } else {
    # fallback: approximate range from change over dt01
    scale_range <- diff(range(x * dt01, na.rm = TRUE))
  }

  if (!is.finite(scale_range) || scale_range <= 0 ||
      !is.finite(dt01)        || dt01 <= 0) {
    return(NULL)
  }

  schemes <- srtm_trajectory_label_schemes()

  thr_tbl <- tibble::tibble(
    slope_thr = c(
      steep_const * scale_range / dt01,   # steep_pos
      flat_const  * scale_range / dt01,   # shallow_pos
      -flat_const * scale_range / dt01,   # flat
      -steep_const * scale_range / dt01,  # shallow_neg
      -Inf                                # steep_neg
    ),
    schemes
  )

  # ---- mean slope per group -----------------------------------------------
  means    <- tapply(x, gg, mean, na.rm = TRUE)
  g_names  <- names(means)

  rank_by_group <- character(length(means))
  names(rank_by_group) <- g_names

  classify_one <- function(m) {
    if (!is.finite(m)) {
      return("flat")
    }
    idx <- which.max(m >= thr_tbl$slope_thr)
    as.character(thr_tbl$rank_label[idx])
  }

  is_unknown_group <- g_names == "Unknown"

  rank_by_group[!is_unknown_group] <- vapply(
    means[!is_unknown_group],
    classify_one,
    character(1)
  )
  rank_by_group[is_unknown_group] <- NA_character_

  # ---- choose base label set (arrows / signs / text) ----------------------
  lab_col <- dplyr::case_match(
    label_scheme,
    "arrows" ~ "arrow_label",
    "signs"  ~ "sign_label",
    "text"   ~ "text_label"
  )

  base_labels <- schemes[[lab_col]]
  names(base_labels) <- as.character(schemes$rank_label)

  base_by_group <- base_labels[rank_by_group]  # named by group
  names(base_by_group) <- names(rank_by_group)
  base_by_group[is_unknown_group] <- "Unknown"

  # ---- expand to full labels with suffixes when needed --------------------
  full_label_by_group <- base_by_group

  unique_bases <- setdiff(
    unique(base_by_group[!is.na(base_by_group)]),
    "Unknown"
  )

  for (b in unique_bases) {
    g_idx <- which(base_by_group == b)
    if (length(g_idx) <= 1L) next

    grp_names <- names(base_by_group)[g_idx]
    ord       <- order(means[grp_names], decreasing = TRUE)
    grp_names <- grp_names[ord]

    # first group keeps bare label
    full_label_by_group[grp_names[1]] <- b

    # subsequent groups get suffixes _a, _b, ...
    if (length(grp_names) > 1L) {
      suffixes <- letters[seq_len(length(grp_names) - 1L)]
      full_label_by_group[grp_names[-1]] <- paste0(b, "_", suffixes)
    }
  }

  full_label_by_group[is_unknown_group] <- "Unknown"

  # ---- map back to rows ---------------------------------------------------
  trajType <- full_label_by_group[as.character(gg)]

  # ---- factor ordering: scheme-based order where possible -----------------
  # unique labels actually present (excluding NA)
  unique_labels <- unique(stats::na.omit(trajType))

  # base_labels already comes from the chosen scheme (arrows/signs/text)
  # defined earlier as:
  #   base_labels <- schemes[[lab_col]]
  # so we can use it directly:
  base_order <- c(base_labels, "Unknown")

  # keep only labels that are actually present
  levels_out <- intersect(base_order, unique_labels)

  # if something weird happens, fall back to present labels
  if (length(levels_out) == 0L) {
    levels_out <- unique_labels
  }

  trajType <- factor(trajType, levels = levels_out, ordered = TRUE)

  attr(trajType, "srtm_slope_thresholds") <- list(
    slope_col           = slope_col,
    group_col           = group_col,
    dt01                = dt01,
    scale_range         = scale_range,
    steep_const         = steep_const,
    flat_const          = flat_const,
    thresholds          = thr_tbl,
    rank_by_group       = rank_by_group,
    label_by_group      = full_label_by_group,
    mean_slope_by_group = means
  )

  trajType
}
