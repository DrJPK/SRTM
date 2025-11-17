test_that("predictPostResponse computes sensible slopes and exp_y2", {
  df <- tibble::tibble(
    baseGroup = factor(c("A", "A", "A", "A",
                         "B", "B", "B", "B")),
    trajGroup = factor(c("X", "X", "Y", "Y",
                         "X", "X", "Y", "Y")),
    y0        = c(0, 0, 0, 0,
                  10, 10, 10, 10),
    y1        = c(10, 10, 20, 20,
                  20, 20, 40, 40),
    y2        = NA_real_
  )

  res <- predictPostResponse(
    data       = df,
    y0         = "y0",
    y1         = "y1",
    y2         = "y2",
    base_group = "baseGroup",
    traj_group = "trajGroup",
    time01     = 1,
    time12     = 1
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("intercept01", "slope01", "exp_y2") %in% names(res)))

  # A-X: 0 -> 10 => slope = 10, intercept = 10
  idx_AX <- res$baseGroup == "A" & res$trajGroup == "X"
  expect_true(all(res$intercept01[idx_AX] == 10))
  expect_true(all(res$slope01[idx_AX] == 10))
  expect_true(all(res$exp_y2[idx_AX] == 20))

  # A-Y: 0 -> 20 => slope = 20, intercept = 20
  idx_AY <- res$baseGroup == "A" & res$trajGroup == "Y"
  expect_true(all(res$intercept01[idx_AY] == 20))
  expect_true(all(res$slope01[idx_AY] == 20))
  expect_true(all(res$exp_y2[idx_AY] == 40))

  # B-X: 10 -> 20 => slope = 10, intercept = 20
  idx_BX <- res$baseGroup == "B" & res$trajGroup == "X"
  expect_true(all(res$intercept01[idx_BX] == 20))
  expect_true(all(res$slope01[idx_BX] == 10))
  expect_true(all(res$exp_y2[idx_BX] == 30))

  # B-Y: 10 -> 40 => slope = 30, intercept = 40
  idx_BY <- res$baseGroup == "B" & res$trajGroup == "Y"
  expect_true(all(res$intercept01[idx_BY] == 40))
  expect_true(all(res$slope01[idx_BY] == 30))
  expect_true(all(res$exp_y2[idx_BY] == 70))
})



test_that("predictPostResponse validates data type", {
  expect_error(
    predictPostResponse(
      data       = 1:10,
      time01     = 1,
      time12     = 1
    ),
    class = "srtm_predict_bad_data"
  )
})

test_that("predictPostResponse errors when required columns are missing", {
  df <- tibble::tibble(
    y0        = 1:3,
    y1        = 2:4,
    baseGroup = factor("A")
    # trajGroup and y2 are missing
  )

  expect_error(
    predictPostResponse(
      data       = df,
      y0         = "y0",
      y1         = "y1",
      y2         = "y2",
      base_group = "baseGroup",
      traj_group = "trajGroup",
      time01     = 1,
      time12     = 1
    ),
    class = "srtm_predict_missing_cols"
  )
})

test_that("predictPostResponse validates time01 and time12", {
  df <- tibble::tibble(
    baseGroup = factor(c("A", "A")),
    trajGroup = factor(c("X", "X")),
    y0        = c(0, 0),
    y1        = c(10, 10),
    y2        = c(NA, NA)
  )

  # time01 invalid
  expect_error(
    predictPostResponse(
      data       = df,
      time01     = 0,
      time12     = 1
    ),
    class = "srtm_predict_bad_time"
  )

  expect_error(
    predictPostResponse(
      data       = df,
      time01     = -1,
      time12     = 1
    ),
    class = "srtm_predict_bad_time"
  )

  expect_error(
    predictPostResponse(
      data       = df,
      time01     = "one",
      time12     = 1
    ),
    class = "srtm_predict_bad_time"
  )

  # time12 invalid
  expect_error(
    predictPostResponse(
      data       = df,
      time01     = 1,
      time12     = 0
    ),
    class = "srtm_predict_bad_time"
  )

  expect_error(
    predictPostResponse(
      data       = df,
      time01     = 1,
      time12     = -1
    ),
    class = "srtm_predict_bad_time"
  )

  expect_error(
    predictPostResponse(
      data       = df,
      time01     = 1,
      time12     = "one"
    ),
    class = "srtm_predict_bad_time"
  )
})
