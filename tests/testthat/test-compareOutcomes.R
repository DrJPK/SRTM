test_that("compareOutcomes computes t-tests per group correctly", {
  df <- tibble::tibble(
    baseGroup = factor(c("A", "A", "A", "A",
                         "B", "B", "B", "B")),
    trajGroup = factor(c("X", "X", "Y", "Y",
                         "X", "X", "Y", "Y")),
    obs       = c(11, 13, 20, 24,
                  30, 34, 40, 46),
    exp       = c(10, 12, 18, 22,
                  28, 32, 38, 44)
  )

  res <- compareOutcomes(
    data        = df,
    obs         = "obs",
    exp         = "exp",
    baseGroups  = "baseGroup",
    trajGroups  = "trajGroup"
  )

  row_AX <- res[res$baseGroup == "A" & res$trajGroup == "X", ]
  row_AY <- res[res$baseGroup == "A" & res$trajGroup == "Y", ]
  row_BX <- res[res$baseGroup == "B" & res$trajGroup == "X", ]
  row_BY <- res[res$baseGroup == "B" & res$trajGroup == "Y", ]

  # mean differences are exact
  expect_equal(row_AX$diff, 1)
  expect_equal(row_AY$diff, 2)
  expect_equal(row_BX$diff, 2)
  expect_equal(row_BY$diff, 2)

  # constant diffs -> t and p undefined, but df = n - 1
  expect_true(is.na(row_AX$t_value))
  expect_true(is.na(row_AY$t_value))
  expect_true(is.na(row_BX$t_value))
  expect_true(is.na(row_BY$t_value))

  expect_true(is.na(row_AX$p))
  expect_true(is.na(row_AY$p))
  expect_true(is.na(row_BX$p))
  expect_true(is.na(row_BY$p))

  expect_equal(row_AX$df, 1)
  expect_equal(row_AY$df, 1)
  expect_equal(row_BX$df, 1)
  expect_equal(row_BY$df, 1)
})


test_that("compareOutcomes supports bare and string column specifications", {
  df <- tibble::tibble(
    baseGroup = factor(c("A", "A", "B", "B")),
    trajGroup = factor(c("X", "X", "X", "Y")),
    y2        = c(10, 12, 20, 30),
    exp_y2    = c(9, 11, 19, 31)
  )

  res1 <- compareOutcomes(
    data        = df,
    obs         = "y2",
    exp         = "exp_y2",
    baseGroups  = "baseGroup",
    trajGroups  = "trajGroup"
  )

  res2 <- compareOutcomes(
    data        = df,
    obs         = y2,
    exp         = exp_y2,
    baseGroups  = baseGroup,
    trajGroups  = trajGroup
  )

  expect_equal(res2$diff, res1$diff)
  expect_equal(res2$df, res1$df)
  # t and p may differ slightly due to floating precision but should match exactly here
  expect_equal(res2$t_value, res1$t_value)
  expect_equal(res2$p, res1$p)
})

test_that("compareOutcomes returns NA stats for groups with too few observations", {
  df <- tibble::tibble(
    baseGroup = factor(c("A", "A", "B")),
    trajGroup = factor(c("X", "X", "X")),
    y2        = c(10, 12, 20),
    exp_y2    = c(9, 11, 19)
  )

  # A-X: 2 rows -> valid; B-X: 1 row -> NA
  res <- compareOutcomes(
    data        = df,
    obs         = "y2",
    exp         = "exp_y2",
    baseGroups  = "baseGroup",
    trajGroups  = "trajGroup"
  )

  row_AX <- res[res$baseGroup == "A" & res$trajGroup == "X", ]
  row_BX <- res[res$baseGroup == "B" & res$trajGroup == "X", ]

  expect_false(any(is.na(row_AX$diff)))
  expect_true(all(is.na(row_BX$t_value)))
  expect_true(all(is.na(row_BX$diff)))
  expect_true(all(is.na(row_BX$p)))
  expect_true(all(is.na(row_BX$df)))
})

test_that("compareOutcomes validates data type and columns", {
  # non-data input
  expect_error(
    compareOutcomes(1:10),
    class = "srtm_compare_bad_data"
  )

  # missing columns
  df_missing <- tibble::tibble(
    baseGroup = factor("A"),
    y2        = 10
    # exp_y2 and trajGroup missing
  )

  expect_error(
    compareOutcomes(
      data        = df_missing,
      obs         = "y2",
      exp         = "exp_y2",
      baseGroups  = "baseGroup",
      trajGroups  = "trajGroup"
    ),
    class = "srtm_compare_missing_cols"
  )

  # non-numeric obs/exp
  df_bad <- tibble::tibble(
    baseGroup = factor(c("A", "A")),
    trajGroup = factor(c("X", "X")),
    y2        = c("ten", "twelve"),
    exp_y2    = c(9, 11)
  )

  expect_error(
    compareOutcomes(
      data        = df_bad,
      obs         = "y2",
      exp         = "exp_y2",
      baseGroups  = "baseGroup",
      trajGroups  = "trajGroup"
    ),
    class = "srtm_compare_non_numeric"
  )
})
