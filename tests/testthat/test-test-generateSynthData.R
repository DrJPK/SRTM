test_that("generateSynthData returns correct basic structure by default", {
  df <- generateSynthData(n = 100, seed = 1234)

  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 100L)

  expect_true(all(c("ID", "y0", "y1", "y2") %in% names(df)))

  expect_s3_class(df$ID, "factor")
  expect_type(df$y0, "double")
  expect_type(df$y1, "double")
  expect_type(df$y2, "double")
})

test_that("generateSynthData respects bounds and decimals", {
  min_val  <- 1
  max_val  <- 5
  decimals <- 2

  df <- generateSynthData(
    n        = 200,
    min      = min_val,
    max      = max_val,
    decimals = decimals,
    seed     = 5678
  )

  # All values within [min, max]
  expect_true(all(df$y0 >= min_val & df$y0 <= max_val, na.rm = TRUE))
  expect_true(all(df$y1 >= min_val & df$y1 <= max_val, na.rm = TRUE))
  expect_true(all(df$y2 >= min_val & df$y2 <= max_val, na.rm = TRUE))

  # Check decimal places using a tolerance
  scale <- 10^decimals
  tol   <- 1e-8

  check_decimals <- function(x) {
    scaled <- x * scale
    diff   <- abs(scaled - round(scaled))
    all(diff < tol, na.rm = TRUE)
  }

  expect_true(check_decimals(df$y0))
  expect_true(check_decimals(df$y1))
  expect_true(check_decimals(df$y2))
})

test_that("generateSynthData returns full df when return_full_df = TRUE", {
  df <- generateSynthData(
    n              = 50,
    seed           = 9999,
    return_full_df = TRUE
  )

  expect_true(all(c(
    "ID", "Group", "y0", "y1", "y2",
    "slope_m", "slope_sd", "slope_m2", "slope_sd2", "affected"
  ) %in% names(df)))

  expect_s3_class(df$Group, "factor")
  expect_type(df$slope_m, "double")
  expect_type(df$slope_sd, "double")
  expect_type(df$slope_m2, "double")
  expect_type(df$slope_sd2, "double")
  expect_type(df$affected, "logical")
})

test_that("effect_direction = 'positive' adjusts slopes as specified", {
  # all individuals affected to make logic clearer
  df <- generateSynthData(
    n              = 200,
    seed           = 1111,
    effect_direction = "positive",
    effect_prob      = 1,
    slopes = list(
      values = c(0.5, -0.5),
      freqs  = c(0.5, 0.5)
    ),
    return_full_df = TRUE
  )

  effect_strength <- 2

  # positive slopes should be multiplied by effect_strength
  pos_idx <- df$slope_m > 0 & df$affected
  expect_true(all(df$slope_m2[pos_idx] == df$slope_m[pos_idx] * effect_strength))

  # negative slopes should be divided by effect_strength
  neg_idx <- df$slope_m < 0 & df$affected
  expect_true(all(df$slope_m2[neg_idx] == df$slope_m[neg_idx] / effect_strength))

  # unaffected (none, since effect_prob = 1), but check logic anyway
  unaffected_idx <- !df$affected
  expect_true(all(df$slope_m2[unaffected_idx] == df$slope_m[unaffected_idx]))
})

test_that("effect_direction = 'negative' adjusts slopes as specified", {
  df <- generateSynthData(
    n                = 200,
    seed             = 2222,
    effect_direction = "negative",
    effect_prob      = 1,
    slopes = list(
      values = c(0.5, -0.5),
      freqs  = c(0.5, 0.5)
    ),
    return_full_df = TRUE
  )

  effect_strength <- 2

  # positive slopes should be divided by effect_strength
  pos_idx <- df$slope_m > 0 & df$affected
  expect_true(all(df$slope_m2[pos_idx] == df$slope_m[pos_idx] / effect_strength))

  # negative slopes should be multiplied by effect_strength
  neg_idx <- df$slope_m < 0 & df$affected
  expect_true(all(df$slope_m2[neg_idx] == df$slope_m[neg_idx] * effect_strength))

  unaffected_idx <- !df$affected
  expect_true(all(df$slope_m2[unaffected_idx] == df$slope_m[unaffected_idx]))
})

test_that("generateSynthData validates arguments and errors with correct classes", {
  # bad n
  expect_error(
    generateSynthData(n = 0),
    class = "srtm_generate_bad_n"
  )

  # bad range
  expect_error(
    generateSynthData(min = 5, max = 1),
    class = "srtm_generate_bad_range"
  )

  # bad init_groups
  expect_error(
    generateSynthData(init_groups = 0),
    class = "srtm_generate_bad_init_groups"
  )

  # slopes length mismatch
  expect_error(
    generateSynthData(
      slopes = list(
        values = c(0.5, 0),
        freqs  = c(0.75, 0.1, 0.15)
      )
    ),
    class = "srtm_generate_bad_slopes"
  )

  # non-numeric slopes
  expect_error(
    generateSynthData(
      slopes = list(
        values = c("a", "b"),
        freqs  = c(0.5, 0.5)
      )
    ),
    class = "srtm_generate_bad_slopes_type"
  )

  # negative freqs
  expect_error(
    generateSynthData(
      slopes = list(
        values = c(0.5, 0),
        freqs  = c(-0.5, 1.5)
      )
    ),
    class = "srtm_generate_bad_slopes_freq"
  )

  # bad effect_strength
  expect_error(
    generateSynthData(effect_strength = 0),
    class = "srtm_generate_bad_effect_strength"
  )

  # bad effect_prob
  expect_error(
    generateSynthData(effect_prob = 1.5),
    class = "srtm_generate_bad_effect_prob"
  )
})

test_that("slopes freqs are normalised with an informative message", {
  # sum of freqs != 1 triggers rlang::inform
  expect_message(
    generateSynthData(
      n      = 10,
      slopes = list(
        values = c(0.5, 0),
        freqs  = c(0.8, 0.8)  # will be renormalised
      ),
      seed = 123
    ),
    "Normalising `slopes\\$freqs` to sum to 1\\."
  )
})

test_that("setting a seed makes generateSynthData reproducible", {
  df1 <- generateSynthData(n = 50, seed = 4242)
  df2 <- generateSynthData(n = 50, seed = 4242)

  expect_identical(df1, df2)
})
