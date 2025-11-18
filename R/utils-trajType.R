#' Compute semantic trajectory types per trajectory group
#'
#' @param data Data frame containing at least a slope column and a group column.
#' @param slope_col Character. Name of the slope column (e.g. "m01").
#' @param group_col Character. Name of the trajectory grouping column
#'   (e.g. "trajGroup").
#' @param label_scheme One of "arrows", "signs", or "text".
#' @param dt01 Numeric > 0. Time between y0 and y1.
#'
#' @keywords internal
srtm_compute_trajType <- function(data,
                                  slope_col    = "m01",
                                  group_col    = "trajGroup",
                                  label_scheme = c("arrows", "signs", "text"),
                                  dt01) {

  label_scheme <- rlang::arg_match(label_scheme)

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_trajType_bad_data"
    )
  }

  if (!is.character(slope_col) || length(slope_col) != 1L) {
    rlang::abort(
      "`slope_col` must be a single character string.",
      class = "srtm_trajType_bad_slope_col"
    )
  }

  if (!is.character(group_col) || length(group_col) != 1L) {
    rlang::abort(
      "`group_col` must be a single character string.",
      class = "srtm_trajType_bad_group_col"
    )
  }

  if (!slope_col %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{slope_col}` not found in `data`."),
      class = "srtm_trajType_missing_slope"
    )
  }

  if (!group_col %in% names(data)) {
    rlang::abort(
      glue::glue("Column `{group_col}` not found in `data`."),
      class = "srtm_trajType_missing_group"
    )
  }

  slopes <- data[[slope_col]]
  groups <- data[[group_col]]

  if (!is.numeric(slopes)) {
    rlang::abort(
      glue::glue("Column `{slope_col}` must be numeric."),
      class = "srtm_trajType_non_numeric_slope"
    )
  }

  # convert groups to factor for consistent level handling
  groups_fac <- as.factor(groups)
  g_levels   <- levels(groups_fac)

  if (length(g_levels) == 0L || all(is.na(groups_fac))) {
    rlang::inform(
      "All trajectory group values are NA; returning NA trajType.",
      class = "srtm_trajType_all_na"
    )
    res <- factor(rep(NA_character_, length(groups_fac)), ordered = TRUE)
    return(res)
  }

  n_g <- length(g_levels)

  # if too many groups, bail but still attach thresholds later
  if (n_g > 5L) {
    rlang::inform(
      glue::glue(
        "More than 5 trajectory groups detected (n = {n_g}); ",
        "semantic trajectory labels will not be assigned."
      ),
      class = "srtm_trajType_too_many_groups"
    )
    res <- factor(rep(NA_character_, length(groups_fac)), ordered = TRUE)
    return(res)
  }

  # ------------------------------------------------------------------------
  # Estimate scale_range from all y-like columns present
  # ------------------------------------------------------------------------
  value_cols <- intersect(c("y0", "y1", "y2", "exp_y2"), names(data))

  if (length(value_cols) == 0L) {
    rlang::abort(
      "No y-columns (`y0`, `y1`, `y2`, `exp_y2`) found to estimate scale_range.",
      class = "srtm_trajType_no_y_cols"
    )
  }

  all_vals <- unlist(data[value_cols], use.names = FALSE)
  all_vals <- all_vals[is.finite(all_vals)]

  if (!length(all_vals)) {
    rlang::abort(
      "No finite values in y-columns to estimate scale_range.",
      class = "srtm_trajType_no_finite_y"
    )
  }

  scale_range <- max(all_vals) - min(all_vals)

  # thresholds object
  thr <- srtm_getSlopeThresholds(scale_range = scale_range, dt01 = dt01)

  # ------------------------------------------------------------------------
  # Compute mean slope per group (using underlying letters, but ordering
  # is irrelevant here because we classify by magnitude + sign).
  # ------------------------------------------------------------------------
  mean_by_group <- tibble::tibble(
    group = g_levels
  ) %>%
    dplyr::mutate(
      mean_slope = purrr::map_dbl(
        group,
        ~ mean(slopes[groups_fac == .x], na.rm = TRUE)
      )
    )

  # If a group is completely NA, mean_slope becomes NA; keep it and label as NA.
  # ------------------------------------------------------------------------
  # Classify each group's mean slope into semantic types
  # ------------------------------------------------------------------------
  mean_by_group <- mean_by_group %>%
    dplyr::mutate(
      trajType = dplyr::case_when(
        is.na(mean_slope) ~ NA_character_,
        mean_slope >= thr$steep_pos          ~ "steep_pos",
        mean_slope >= thr$flat_pos           ~ "shallow_pos",
        mean_slope <= thr$steep_neg          ~ "steep_neg",
        mean_slope <= thr$flat_neg           ~ "shallow_neg",
        TRUE                                 ~ "flat"
      )
    )

  # map type -> label based on scheme
  mean_by_group <- mean_by_group %>%
    dplyr::mutate(
      trajLabel = dplyr::case_when(
        label_scheme == "arrows" & trajType == "steep_pos"   ~ "↟",
        label_scheme == "arrows" & trajType == "shallow_pos" ~ "↑",
        label_scheme == "arrows" & trajType == "flat"        ~ "→",
        label_scheme == "arrows" & trajType == "shallow_neg" ~ "↓",
        label_scheme == "arrows" & trajType == "steep_neg"   ~ "↡",

        label_scheme == "signs"  & trajType == "steep_pos"   ~ "++",
        label_scheme == "signs"  & trajType == "shallow_pos" ~ "+",
        label_scheme == "signs"  & trajType == "flat"        ~ "—",
        label_scheme == "signs"  & trajType == "shallow_neg" ~ "-",
        label_scheme == "signs"  & trajType == "steep_neg"   ~ "--",

        label_scheme == "text"   & trajType == "steep_pos"   ~ "steep_pos",
        label_scheme == "text"   & trajType == "shallow_pos" ~ "shallow_pos",
        label_scheme == "text"   & trajType == "flat"        ~ "flat",
        label_scheme == "text"   & trajType == "shallow_neg" ~ "shallow_neg",
        label_scheme == "text"   & trajType == "steep_neg"   ~ "steep_neg",

        TRUE ~ NA_character_
      )
    )

  # ------------------------------------------------------------------------
  # Join back to original rows by group
  # ------------------------------------------------------------------------
  map_tbl <- mean_by_group %>%
    dplyr::select(group, trajType, trajLabel)

  df_idx <- tibble::tibble(
    row   = seq_along(groups_fac),
    group = as.character(groups_fac)
  )

  joined <- df_idx %>%
    dplyr::left_join(map_tbl, by = "group") %>%
    dplyr::arrange(row)

  # final factor with an ordered level structure for "arrows" scheme
  if (label_scheme == "arrows") {
    lvl <- c("↟", "↑", "→", "↓", "↡")
  } else if (label_scheme == "signs") {
    lvl <- c("++", "+", "—", "-", "--")
  } else {
    # text
    lvl <- c("steep_pos", "shallow_pos", "flat", "shallow_neg", "steep_neg")
  }

  traj_factor <- factor(joined$trajLabel, levels = lvl, ordered = TRUE)

  # attach thresholds as attribute
  attr(traj_factor, "srtm_slope_thresholds") <- thr

  traj_factor
}
