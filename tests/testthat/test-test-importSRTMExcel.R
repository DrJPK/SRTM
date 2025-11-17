test_that("importSRTMExcel errors if path is missing in non-interactive mode", {
  skip_if_not_installed("readxl")

  expect_error(
    importSRTMExcel(
      path        = NULL,
      interactive = FALSE,
      ID          = "ID",
      y0          = "y0",
      y1          = "y1",
      y2          = "y2"
    ),
    class = "srtm_import_no_path"
  )
})

test_that("importSRTMExcel errors if ID/y0/y1/y2 are missing in non-interactive mode", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # minimal dummy data
  df_raw <- tibble::tibble(
    StudentID = paste0("S", 1:5),
    Historical = 1:5,
    Baseline   = 2:6,
    Post       = 3:7
  )

  path <- file.path(tempdir(), "srtm_import_no_id.xlsx")
  writexl::write_xlsx(df_raw, path)

  # missing ID
  expect_error(
    importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = NULL,
      y0          = "Historical",
      y1          = "Baseline",
      y2          = "Post"
    ),
    class = "srtm_import_missing_ID"
  )

  # missing y0
  expect_error(
    importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = NULL,
      y1          = "Baseline",
      y2          = "Post"
    ),
    class = "srtm_import_missing_y0"
  )

  # missing y1
  expect_error(
    importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = "Historical",
      y1          = NULL,
      y2          = "Post"
    ),
    class = "srtm_import_missing_y1"
  )

  # missing y2
  expect_error(
    importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = "Historical",
      y1          = "Baseline",
      y2          = NULL
    ),
    class = "srtm_import_missing_y2"
  )
})

test_that("importSRTMExcel imports and renames columns correctly", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID = paste0("S", 1:5),
    Historical = c(10, 12, 14, 16, 18),  # y0
    Baseline   = c(11, 13, 15, 17, 19),  # y1
    Post       = c(12, 14, 16, 18, 20)   # y2
  )

  path <- file.path(tempdir(), "srtm_import_basic.xlsx")
  writexl::write_xlsx(df_raw, path)

  df <- importSRTMExcel(
    path        = path,
    interactive = FALSE,
    ID          = "StudentID",
    y0          = "Historical",
    y1          = "Baseline",
    y2          = "Post"
  )

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("ID", "y0", "y1", "y2") %in% names(df)))

  expect_s3_class(df$ID, "factor")
  expect_type(df$y0, "double")
  expect_type(df$y1, "double")
  expect_type(df$y2, "double")

  expect_equal(nrow(df), nrow(df_raw))
})

test_that("importSRTMExcel handles extra_vars and coerces them to factors", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID = paste0("S", 1:5),
    Historical = c(10, 12, 14, 16, 18),
    Baseline   = c(11, 13, 15, 17, 19),
    Post       = c(12, 14, 16, 18, 20),
    Class      = c("A", "A", "B", "B", "C"),
    Gender     = c("F", "M", "F", "M", "F")
  )

  path <- file.path(tempdir(), "srtm_import_extra.xlsx")
  writexl::write_xlsx(df_raw, path)

  df <- importSRTMExcel(
    path        = path,
    interactive = FALSE,
    ID          = "StudentID",
    y0          = "Historical",
    y1          = "Baseline",
    y2          = "Post",
    extra_vars  = c("Class", "Gender")
  )

  expect_true(all(c("Class", "Gender") %in% names(df)))
  expect_s3_class(df$Class, "factor")
  expect_s3_class(df$Gender, "factor")
})

test_that("importSRTMExcel warns when extra_vars are not present", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID = paste0("S", 1:5),
    Historical = c(10, 12, 14, 16, 18),
    Baseline   = c(11, 13, 15, 17, 19),
    Post       = c(12, 14, 16, 18, 20)
  )

  path <- file.path(tempdir(), "srtm_import_missing_extra.xlsx")
  writexl::write_xlsx(df_raw, path)

  expect_warning(
    df <- importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = "Historical",
      y1          = "Baseline",
      y2          = "Post",
      extra_vars  = c("Class", "Gender")
    ),
    class = "srtm_import_missing_extra_vars"
  )

  # Should still get a valid basic df
  expect_true(all(c("ID", "y0", "y1", "y2") %in% names(df)))
})

test_that("importSRTMExcel converts default missing tokens to NA", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID  = paste0("S", 1:6),
    Historical = c("10", "NA", ".", "12", "Missing", " "),
    Baseline   = c("11", "N/A", "-", "13", "missing", ""),
    Post       = c("12", "na", "n/a", "14", ".", "16")
  )

  path <- file.path(tempdir(), "srtm_import_missing_tokens_default.xlsx")
  writexl::write_xlsx(df_raw, path)

  expect_warning(
    df <- importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = "Historical",
      y1          = "Baseline",
      y2          = "Post"
    ),
    class = "srtm_import_na_values"
  )

  # y columns should be numeric
  expect_type(df$y0, "double")
  expect_type(df$y1, "double")
  expect_type(df$y2, "double")

  # Default missing tokens should have produced some NA values
  expect_true(sum(is.na(df$y0)) >= 2)
  expect_true(sum(is.na(df$y1)) >= 2)
  expect_true(sum(is.na(df$y2)) >= 1)
})

test_that("importSRTMExcel respects custom missing_tokens argument", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID  = paste0("S", 1:5),
    Historical = c("10", "Not tested", "12", "Absent", "14"),
    Baseline   = c("11", "Not tested", "13", "Absent", "15"),
    Post       = c("12", "Not tested", "14", "Absent", "16")
  )

  path <- file.path(tempdir(), "srtm_import_custom_missing_tokens.xlsx")
  writexl::write_xlsx(df_raw, path)

  expect_warning(
    df <- importSRTMExcel(
      path           = path,
      interactive    = FALSE,
      ID             = "StudentID",
      y0             = "Historical",
      y1             = "Baseline",
      y2             = "Post",
      missing_tokens = c("Not tested", "Absent")
    ),
    class = "srtm_import_na_values"
  )

  # y columns should be numeric
  expect_type(df$y0, "double")
  expect_type(df$y1, "double")
  expect_type(df$y2, "double")

  # Custom tokens should have become NA
  expect_equal(sum(is.na(df$y0)), 2)
  expect_equal(sum(is.na(df$y1)), 2)
  expect_equal(sum(is.na(df$y2)), 2)
})

test_that("importSRTMExcel handles non-numeric y values with warning", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  df_raw <- tibble::tibble(
    StudentID  = paste0("S", 1:5),
    Historical = c("10", "12", "bad", "16", "18"),   # 'bad' is non-numeric
    Baseline   = c("11", "13", "15", "oops", "19"),  # 'oops' non-numeric
    Post       = c("12", "oops", "16", "18", "20")   # 'oops' non-numeric
  )

  path <- file.path(tempdir(), "srtm_import_bad_types.xlsx")
  writexl::write_xlsx(df_raw, path)

  expect_warning(
    df <- importSRTMExcel(
      path        = path,
      interactive = FALSE,
      ID          = "StudentID",
      y0          = "Historical",
      y1          = "Baseline",
      y2          = "Post"
    ),
    class = "srtm_import_na_values"
  )

  # We should still get a valid tibble with numeric y columns
  expect_s3_class(df, "tbl_df")
  expect_type(df$y0, "double")
  expect_type(df$y1, "double")
  expect_type(df$y2, "double")

  # And at least one NA introduced because of bad strings
  expect_true(any(is.na(df$y0)))
  expect_true(any(is.na(df$y1)))
  expect_true(any(is.na(df$y2)))
})

