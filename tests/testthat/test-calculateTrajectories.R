test_that("calculateTrajectories computes correct slopes for simple data", {
  df <- tibble::tibble(
    ID = paste0("S", 1:4),
    y0 = c(10, 20, 30, 40),
    y1 = c(20, 20, 50, 40)
  )

  # time_period = 10 units
  slopes <- calculateTrajectories(
    data        = df,
    y0          = "y0",
    y1          = "y1",
    time_period = 10,
    interactive = FALSE
  )

  expect_type(slopes, "double")
  expect_length(slopes, nrow(df))

  # (y1 - y0) / 10
  expected <- c(1, 0, 2, 0)
  expect_equal(slopes, expected)
})

test_that("calculateTrajectories handles NA values by propagating NA in slopes", {
  df <- tibble::tibble(
    ID = paste0("S", 1:4),
    y0 = c(10, NA, 30, 40),
    y1 = c(20, 25, NA, 40)
  )

  slopes <- calculateTrajectories(
    data        = df,
    y0          = "y0",
    y1          = "y1",
    time_period = 5,
    interactive = FALSE
  )

  expect_true(is.na(slopes[2]))
  expect_true(is.na(slopes[3]))
  expect_equal(slopes[1], (20 - 10) / 5)
  expect_equal(slopes[4], (40 - 40) / 5)
})

test_that("calculateTrajectories validates data and column types", {
  # non-data.frame
  expect_error(
    calculateTrajectories(
      data        = 1:10,
      time_period = 10,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_bad_data"
  )

  df <- tibble::tibble(
    ID = paste0("S", 1:3),
    y0 = c(1, 2, 3),
    y1 = c(4, 5, 6)
  )

  # missing y0 column
  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "not_here",
      y1          = "y1",
      time_period = 10,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_missing_col"
  )

  # missing y1 column
  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "y0",
      y1          = "not_here",
      time_period = 10,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_missing_col"
  )

  # non-numeric y0
  df_char <- tibble::tibble(
    y0 = c("a", "b", "c"),
    y1 = c(1, 2, 3)
  )

  expect_error(
    calculateTrajectories(
      data        = df_char,
      y0          = "y0",
      y1          = "y1",
      time_period = 10,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_non_numeric"
  )

  # non-numeric y1
  df_char2 <- tibble::tibble(
    y0 = c(1, 2, 3),
    y1 = c("a", "b", "c")
  )

  expect_error(
    calculateTrajectories(
      data        = df_char2,
      y0          = "y0",
      y1          = "y1",
      time_period = 10,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_non_numeric"
  )
})

test_that("calculateTrajectories requires time_period when interactive = FALSE", {
  df <- tibble::tibble(
    y0 = c(1, 2, 3),
    y1 = c(2, 3, 4)
  )

  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "y0",
      y1          = "y1",
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_missing_time"
  )
})

test_that("calculateTrajectories validates time_period when supplied", {
  df <- tibble::tibble(
    y0 = c(1, 2, 3),
    y1 = c(2, 3, 4)
  )

  # zero or negative
  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "y0",
      y1          = "y1",
      time_period = 0,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_bad_time_period"
  )

  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "y0",
      y1          = "y1",
      time_period = -5,
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_bad_time_period"
  )

  # non-numeric
  expect_error(
    calculateTrajectories(
      data        = df,
      y0          = "y0",
      y1          = "y1",
      time_period = "ten",
      interactive = FALSE
    ),
    class = "srtm_calcTrajectories_bad_time_period"
  )
})

test_that("calculateTrajectories works with non-default y0 and y1 names", {
  df <- tibble::tibble(
    baseline = c(10, 20, 30),
    followup = c(25, 35, 45)
  )

  slopes <- calculateTrajectories(
    data        = df,
    y0          = "baseline",
    y1          = "followup",
    time_period = 5,
    interactive = FALSE
  )

  expected <- (df$followup - df$baseline) / 5
  expect_equal(slopes, expected)
})
