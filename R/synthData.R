
#' Generate synthetic self-referenced trajectory data
#'
#' @description
#' Generate a synthetic longitudinal dataset suitable for demonstrating
#' self-referenced trajectory modelling. The function simulates baseline
#' scores and subsequent time points under group-specific distributions,
#' heterogeneous slopes, and an optional intervention effect.
#'
#' @param n Integer. Number of individuals to simulate.
#' @param min,max Numeric. Minimum and maximum possible values on the
#'   outcome scale. Used both as bounds for simulated scores and to
#'   define the scale range.
#' @param decimals Integer. Number of decimal places to which simulated
#'   scores are rounded.
#' @param init_groups Integer. Number of initial latent groups used to
#'   define the baseline (\\code{y0}) means and standard deviations.
#' @param effect_strength Numeric scalar. Multiplicative factor controlling
#'   how strongly the slope is changed for affected individuals.
#' @param effect_direction Character. Direction of the effect on the slope,
#'   either \\code{"positive"} or \\code{"negative"}. See Details.
#' @param effect_prob Numeric in \\code{[0, 1]}. Probability that an
#'   individual is affected by the intervention (i.e., has a modified slope).
#' @param slopes A list with components \\code{values} and \\code{freqs}
#'   giving the possible slope values and their corresponding probabilities.
#'   Both components are coerced to numeric vectors and \\code{freqs} is
#'   normalised to sum to 1 if necessary.
#' @param seed Optional numeric seed for the random number generator. If
#'   missing, a seed is generated and reported via an rlang message.
#' @param return_full_df Logical. If \\code{FALSE} (default), returns only
#'   \\code{ID}, \\code{y0}, \\code{y1}, and \\code{y2}. If \\code{TRUE},
#'   returns additional columns with group membership and trajectory
#'   parameters.
#'
#'   @details
#' Baseline scores \\code{y0} are drawn from group-specific normal
#' distributions, with group means and standard deviations defined within
#' the \\code{[min, max]} scale range. Heteroscedastic noise is used so
#' that variability is largest near the middle of the scale and smaller
#' near the bounds.
#'
#' Individual slopes are sampled from \\code{slopes$values} with
#' probabilities \\code{slopes$freqs}. The second time point \\code{y1}
#' is generated as \\code{y0} plus a slope-driven change and heteroscedastic
#' noise. The third time point \\code{y2} is generated using either the
#' original slope (for unaffected individuals) or a modified slope (for
#' affected individuals), where the modification depends on
#' \\code{effect_strength} and \\code{effect_direction}. All scores are
#' constrained to the \\code{[min, max]} range. It is assumed that the time
#' between \\code{y0} and \\code{y1} is the same as between \\code{y1} and
#' \\code{y2}.
#'
#' @return
#' A tibble. By default, a tibble with columns:
#' \\code{ID}, \\code{y0}, \\code{y1}, \\code{y2}. If
#' \\code{return_full_df = TRUE}, additional columns are included:
#' \\code{Group}, \\code{slope_m}, \\code{slope_sd}, \\code{slope_m2},
#' \\code{slope_sd2}, and \\code{affected}.
#'
#' @examples
#' # Basic usage
#' df <- generateSynthData(n = 200)
#'
#' # Return full parameter set for inspection
#' df_full <- generateSynthData(
#'   n = 200,
#'   effect_direction = "negative",
#'   return_full_df   = TRUE
#' )
#'
#' @export

generateSynthData <- function(n = 100,
                              min = 1,
                              max = 5,
                              decimals = 1,
                              init_groups = 2,
                              effect_strength = 2,
                              effect_direction = c("positive","negative"),
                              effect_prob = 0.75,
                              slopes = list(
                                "values" = list(0.5 , 0, -0.5),
                                "freqs" = list(0.75,0.1,0.15)
                              ),
                              seed,
                              return_full_df = FALSE){

  if (!is.numeric(n) || length(n) != 1L || n <= 0) {
    rlang::abort("`n` must be a single positive numeric value.",
                 class = "srtm_generate_bad_n")
  }

  if (!is.numeric(min) || !is.numeric(max) || min >= max) {
    rlang::abort("`min` must be strictly less than `max` and both numeric.",
                 class = "srtm_generate_bad_range")
  }

  if (!is.numeric(init_groups) || length(init_groups) != 1L || init_groups < 1) {
    rlang::abort("`init_groups` must be a single positive integer.",
                 class = "srtm_generate_bad_init_groups")
  }

  init_groups <- as.integer(init_groups)
  slope_vals <- unlist(slopes$values)
  slope_freq <- unlist(slopes$freqs)

  if (length(slope_vals) != length(slope_freq)) {
    rlang::abort(
      "`slopes$values` and `slopes$freqs` must have the same length.",
      class = "srtm_generate_bad_slopes"
    )
  }

  if (!is.numeric(slope_vals) || !is.numeric(slope_freq)) {
    rlang::abort(
      "`slopes$values` and `slopes$freqs` must be numeric.",
      class = "srtm_generate_bad_slopes_type"
    )
  }

  if (any(slope_freq < 0)) {
    rlang::abort(
      "`slopes$freqs` must be non-negative.",
      class = "srtm_generate_bad_slopes_freq"
    )
  }

  if (!isTRUE(all.equal(sum(slope_freq), 1, tolerance = 1e-6))) {
    slope_freq <- slope_freq / sum(slope_freq)
    rlang::inform(
      "Normalising `slopes$freqs` to sum to 1.",
      class = "srtm_generate_slopes_renorm"
    )
  }

  effect_direction <- rlang::arg_match(effect_direction)

  if (!is.numeric(effect_strength) || length(effect_strength) != 1L ||
      effect_strength <= 0) {
    rlang::abort(
      "`effect_strength` must be a single positive numeric value.",
      class = "srtm_generate_bad_effect_strength"
    )
  }

  if (!is.numeric(effect_prob) || length(effect_prob) != 1L ||
      effect_prob < 0 || effect_prob > 1) {
    rlang::abort(
      "`effect_prob` must be a single numeric in [0, 1].",
      class = "srtm_generate_bad_effect_prob"
    )
  }

  if (missing(seed)) {
    seed <- stats::runif(1, 1, 1e9)
    rlang::inform(
      message = glue::glue(
        "No `seed` supplied. Using generated seed: {round(seed)}"
      ),
      class   = "srtm_generate_seed_auto"
    )
  }
  set.seed(seed)

  range <- max - min

  params <- tibble::tibble(
    "Group" = factor(LETTERS[seq_len(init_groups)],
                     levels = LETTERS[seq_len(init_groups)]),
    "Mean"  = round(
      stats::runif(init_groups,
                   min + 0.2 * range,
                   max - 0.2 * range),
      decimals + 1
    ),
    "SD"    = round(
      stats::runif(init_groups,
                   0.15 * range,
                   0.40 * range),
      decimals + 1
    )
  )

  probs <- numeric(init_groups)
  remaining <- 1

  for (i in seq_len(init_groups)) {
    if (i < init_groups) {
      probs[i] <- stats::runif(1, 0, remaining)
      remaining <- remaining - probs[i]
    } else {
      probs[i] <- remaining
    }
  }

  probs <- probs / sum(probs)
  params$Probs <- probs

  id_vec <- sprintf("ID%06d", seq_len(n))

  df <- tibble::tibble(
    "ID"    = factor(id_vec, levels = id_vec),
    "Group" = sample(params$Group,
                     size    = n,
                     replace = TRUE,
                     prob    = params$Probs)
  ) %>%
    dplyr::left_join(params, by = "Group") %>%
    dplyr::mutate(
      y0_raw = stats::rnorm(dplyr::n(), mean = Mean, sd = SD),
      y0_raw = pmin(pmax(y0_raw, min), max),
      y0     = round(y0_raw, decimals),
      pos = (y0 - min) / range,
      sd_min = 0.05 * range,
      sd_max = 0.25 * range,
      slope_sd = sd_min + (sd_max - sd_min) * (1 - 4 * (pos - 0.5)^2),
      slope_m = sample(
        x      = slope_vals,
        size   = dplyr::n(),
        replace = TRUE,
        prob   = slope_freq
      ),
      f_y1 = stats::rnorm(
        dplyr::n(),
        mean = slope_m * 1,
        sd   = slope_sd
      ),
      y1_raw = y0 + f_y1,
      y1_raw = pmin(pmax(y1_raw, min), max),
      y1     = round(y1_raw, decimals)
    )

  df <- df %>%
    dplyr::mutate(
      pos2   = (y1 - min) / range,
      sd_min2 = 0.05 * range,
      sd_max2 = 0.25 * range,
      slope_sd2 = sd_min2 + (sd_max2 - sd_min2) * (1 - 4 * (pos2 - 0.5)^2),
      affected = stats::rbinom(dplyr::n(), size = 1, prob = effect_prob) == 1L
    )

  if (effect_direction == "positive") {
    df <- df %>%
      dplyr::mutate(
        slope_m2 = dplyr::case_when(
          !affected ~ slope_m,
          affected & slope_m > 0 ~ slope_m * effect_strength,   # steeper positive
          affected & slope_m < 0 ~ slope_m / effect_strength,   # less steep negative
          TRUE ~ slope_m
        )
      )
  } else { # effect_direction == "negative"
    df <- df %>%
      dplyr::mutate(
        slope_m2 = dplyr::case_when(
          !affected ~ slope_m,
          affected & slope_m > 0 ~ slope_m / effect_strength,   # less steep positive
          affected & slope_m < 0 ~ slope_m * effect_strength,   # steeper negative
          TRUE ~ slope_m
        )
      )
  }

  df <- df %>%
    dplyr::mutate(
      # f for t = 1 from y1 to y2
      f_y2 = stats::rnorm(
        dplyr::n(),
        mean = slope_m2 * 1,      # increment over this interval
        sd   = slope_sd2          # heteroscedastic noise
      ),
      y2_raw = y1 + f_y2,
      y2_raw = pmin(pmax(y2_raw, min), max),
      y2     = round(y2_raw, decimals)
    )

  if(return_full_df){
    df <- df %>%
      dplyr::select(
        ID,
        Group,
        y0,
        y1,
        y2,
        slope_m,
        slope_sd,
        slope_m2,
        slope_sd2,
        affected
      )
  }else{
    df <- df %>%
      dplyr::select(
        ID,
        y0,
        y1,
        y2
      )
  }

  df

}
