#' Trajectory label schemes for SRTM
#'
#' @keywords internal
srtm_trajectory_label_schemes <- function() {
  tibble::tibble(
    rank_label  = factor(
      c("steep_pos", "shallow_pos", "flat", "shallow_neg", "steep_neg"),
      levels = c("steep_pos", "shallow_pos", "flat", "shallow_neg", "steep_neg")
    ),
    arrow_label = c("↟", "↑", "→", "↓", "↡"),
    sign_label  = c("++", "+", "—", "-", "--"),
    text_label  = as.character(rank_label),
    report_description = c(
      "a steeply increasing trajectory",
      "a moderately increasing trajectory",
      "a stable (flat) trajectory",
      "a moderately decreasing trajectory",
      "a steeply decreasing trajectory"
    )
  )
}

#' Get a trajectory label scheme as a character vector
#'
#' @param scheme A character scalar naming the column in
#'   [srtm_trajectory_label_schemes()] to use as labels. Typical options are
#'   \code{"arrow_label"}, \code{"sign_label"}, or \code{"text_label"}.
#'
#' @return A character vector of labels in the canonical SRMT ordering
#'   (steep_pos, shallow_pos, flat, shallow_neg, steep_neg).
#'
#' @keywords internal
#' @noRd
getLabelScheme <- function(scheme = c("arrow_label", "sign_label", "text_label")) {
  scheme <- rlang::arg_match(scheme)
  x      <- srtm_trajectory_label_schemes()
  dplyr::pull(x, .data[[scheme]])
}
