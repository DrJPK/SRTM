#' Compute slope thresholds for trajectory typing
#'
#' @param scale_range Numeric > 0. Approximate scale range of the outcome
#'   (e.g., max(y*) - min(y*)).
#' @param dt01 Numeric > 0. Time between y0 and y1.
#' @param steep_frac Numeric > 0. Fraction of scale_range per time unit
#'   used to define "steep" slopes.
#' @param flat_frac Numeric > 0. Fraction of scale_range per time unit
#'   used to define the central "flat" band.
#'
#' @keywords internal
srtm_getSlopeThresholds <- function(scale_range,
                                    dt01,
                                    steep_frac = 0.25,
                                    flat_frac  = 0.05) {

  if (!is.numeric(scale_range) || length(scale_range) != 1L || scale_range <= 0) {
    rlang::abort(
      "`scale_range` must be a single positive numeric value.",
      class = "srtm_slopeThr_bad_scale_range"
    )
  }

  if (!is.numeric(dt01) || length(dt01) != 1L || dt01 <= 0) {
    rlang::abort(
      "`dt01` must be a single positive numeric value.",
      class = "srtm_slopeThr_bad_dt01"
    )
  }

  steep_thr <- steep_frac * scale_range / dt01
  flat_thr  <- flat_frac  * scale_range / dt01

  list(
    scale_range = scale_range,
    dt01        = dt01,
    steep_frac  = steep_frac,
    flat_frac   = flat_frac,
    steep_pos   = steep_thr,
    shallow_pos = flat_thr,
    flat_pos    = flat_thr,
    flat_neg    = -flat_thr,
    shallow_neg = -flat_thr,
    steep_neg   = -steep_thr
  )
}
