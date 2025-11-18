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
    text_label  = as.character(rank_label)
  )
}
