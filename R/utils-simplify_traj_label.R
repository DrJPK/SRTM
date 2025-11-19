#' Simplify trajectory labels by removing suffixes (e.g., "_a", "_b")
#'
#' @description
#' Given a vector of trajectory labels (e.g., "↑", "↑_a", "↑_b",
#' "shallow_pos_b", "++_a"), return the *base* label without suffixes.
#'
#' This is useful for plot faceting or high-level summaries where multiple
#' trajectory sub-groups (created because several k-means/density groups fell into
#' the same semantic band) should be displayed as one category.
#'
#' @param x A character vector or factor containing trajectory labels produced by
#'   `srtm_compute_trajType()`.
#' @param as_factor Logical (default `FALSE`). If `TRUE`, returns an ordered
#'   factor using the natural trajectory order for arrows, signs, or text.
#'
#' @return A character vector (default) or ordered factor with suffixes removed.
#'
#' @examples
#' simplify_traj_label(c("↑", "↑_a", "↑_b", "→", "↡_c"))
#' # [1] "↑" "↑" "↑" "→" "↡"
#'
#' @export
simplify_traj_label <- function(x, as_factor = FALSE) {

  # Coerce factors to character internally
  x_chr <- as.character(x)

  # Identify "Unknown" (preserve exactly)
  is_unknown <- x_chr %in% c("Unknown", NA_character_)

  # Strip suffixes: everything after first underscore
  # (handles multi-character bases like "shallow_pos_b")
  base <- sub("_[a-zA-Z]+$", "", x_chr)

  # Restore Unknown labels untouched
  base[is_unknown] <- "Unknown"

  if (!as_factor) {
    return(base)
  }

  # --- construct ordered factor ------------------------------------------
  schemes <- srtm_trajectory_label_schemes()

  # all possible base labels for arrows, signs, text:
  base_arrow <- schemes$arrow_label
  base_sign  <- schemes$sign_label
  base_text  <- as.character(schemes$rank_label)

  # Detect which scheme we are in
  if (all(base %in% c(base_arrow, "Unknown"))) {
    levels_out <- c(base_arrow, "Unknown")
  } else if (all(base %in% c(base_sign, "Unknown"))) {
    levels_out <- c(base_sign, "Unknown")
  } else if (all(base %in% c(base_text, "Unknown"))) {
    levels_out <- c(base_text, "Unknown")
  } else {
    # fallback: alphabetical
    levels_out <- sort(unique(base))
  }

  factor(base, levels = levels_out, ordered = TRUE)
}
