#' Import SRTM Data from an Excel Spreadsheet
#'
#' @description
#' Reads a teacher or researcher provided Excel file containing Self-Referenced Trajectory
#' Modelling (SRTM) data and converts it into a standardised dataframe with the
#' required variables:
#' * `ID` – learner or participant identifier (factor)
#' * `y0` – historical data
#' * `y1` – baseline data
#' * `y2` – post data
#'
#' The function supports interactive selection of the file and relevant columns,
#' making it suitable for teachers and non-technical users. Any additional
#' variables named via `extra_vars` are automatically coerced to factors.
#'
#' @param path Optional. Path to the Excel file. If `NULL` and
#'   `interactive = TRUE`, a file chooser dialog is opened.
#' @param sheet Optional. Sheet name or index to read, passed to
#'   [readxl::read_excel()]. If `NULL`, the first sheet is used.
#' @param interactive Logical. If `TRUE` (default), the user may be prompted to
#'   choose the file and/or select the appropriate columns interactively.
#' @param ID Optional. Name of the column containing learner identifiers. If
#'   `NULL` and interactive mode is enabled, the user is prompted to choose.
#' @param y0 Optional. Name of the column containing **historical data**. If
#'   `NULL` and interactive mode is enabled, the user is prompted to choose.
#' @param y1 Optional. Name of the column containing **baseline** data. If
#'   `NULL` and interactive mode is enabled, the user is prompted to choose.
#' @param y2 Optional. Name of the column containing **post** data. If `NULL`
#'   and interactive mode is enabled, the user is prompted to choose.
#' @param extra_vars Optional character vector or list of additional variable
#'   names to import. Any that are present in the dataset are included and
#'   coerced to factors to support downstream grouping or filtering.
#'
#' @details
#' The function wraps [readxl::read_excel()] and provides an interactive column
#' selection process based on `utils::menu()`. Users can load spreadsheets
#' exported from school systems or teacher-created Excel files without requiring
#' exact variable naming.
#'
#' The returned dataframe always contains **ID**, **y0**, **y1**, and **y2**
#' with standard names suitable for downstream SRTM analysis functions.
#'
#' @return
#' A tibble containing:
#' \itemize{
#'   \item `ID` — factor (participant identifier)
#'   \item `y0` — numeric historical data
#'   \item `y1` — numeric baseline data
#'   \item `y2` — numeric post data
#'   \item Additional factor variables if `extra_vars` were provided
#' }
#'
#' @examples
#' \dontrun{
#' # Interactive import (user selects file + columns)
#' df <- import_srtm_excel()
#'
#' # Non-interactive import with explicit column names
#' df <- import_srtm_excel(
#'   path = "teacher_data.xlsx",
#'   ID   = "StudentID",
#'   y0   = "Prior_Score",
#'   y1   = "Baseline",
#'   y2   = "Post",
#'   extra_vars = c("Class", "Teacher")
#' )
#' }
#'
#' @export

import_srtm_excel <- function(path = NULL,
                              sheet = NULL,
                              interactive = TRUE,
                              ID = NULL,
                              y0 = NULL,
                              y1 = NULL,
                              y2 = NULL,
                              extra_vars = NULL) {

  # --- basic checks --------------------------------------------------------
  if (!interactive && is.null(path)) {
    rlang::abort(
      "`path` must be supplied when `interactive = FALSE`.",
      class = "srtm_import_no_path"
    )
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    rlang::abort(
      "Package `readxl` is required to import Excel files.",
      class = "srtm_import_missing_readxl"
    )
  }

  # --- choose file interactively if needed ---------------------------------
  if (is.null(path) && interactive) {
    rlang::inform("Please choose the Excel file to import.")
    path <- utils::file.choose()
  }

  if (is.null(path) || !file.exists(path)) {
    rlang::abort(
      glue::glue("File `{path}` does not exist or could not be found."),
      class = "srtm_import_bad_path"
    )
  }

  # --- read the data -------------------------------------------------------
  raw <- readxl::read_excel(path, sheet = sheet)

  choose_col <- function(df, purpose) {
    cols <- names(df)
    rlang::inform(
      glue::glue(
        "Select the column to use for {purpose}:"
      )
    )
    idx <- utils::menu(cols, graphics = FALSE)
    if (idx == 0) {
      rlang::abort(
        glue::glue("No column selected for {purpose}."),
        class = "srtm_import_no_column"
      )
    }
    cols[[idx]]
  }

  # --- resolve core column names -------------------------------------------
  if (is.null(ID)) {
    if (interactive) {
      ID <- choose_col(raw, "ID")
    } else {
      rlang::abort(
        "`ID` column name must be supplied when `interactive = FALSE`.",
        class = "srtm_import_missing_ID"
      )
    }
  }

  if (is.null(y0)) {
    if (interactive) {
      y0 <- choose_col(raw, "y0 (historical data)")
    } else {
      rlang::abort(
        "`y0` column name must be supplied when `interactive = FALSE`.",
        class = "srtm_import_missing_y0"
      )
    }
  }

  if (is.null(y1)) {
    if (interactive) {
      y1 <- choose_col(raw, "y1 (baseline)")
    } else {
      rlang::abort(
        "`y1` column name must be supplied when `interactive = FALSE`.",
        class = "srtm_import_missing_y1"
      )
    }
  }

  if (is.null(y2)) {
    if (interactive) {
      y2 <- choose_col(raw, "y2 (post data)")
    } else {
      rlang::abort(
        "`y2` column name must be supplied when `interactive = FALSE`.",
        class = "srtm_import_missing_y2"
      )
    }
  }

  core_cols <- c(ID, y0, y1, y2)

  # --- extra vars handling -------------------------------------------------
  if (!is.null(extra_vars)) {
    extra_vars <- unique(as.character(unlist(extra_vars)))
    extra_vars <- extra_vars[extra_vars %in% names(raw)]
    if (length(extra_vars) == 0L) {
      rlang::warn(
        "None of the specified `extra_vars` are present in the data.",
        class = "srtm_import_missing_extra_vars"
      )
      extra_vars <- NULL
    }
  }

  # --- build cleaned dataframe ---------------------------------------------
  df <- raw %>%
    dplyr::select(dplyr::all_of(c(core_cols, extra_vars))) %>%
    dplyr::rename(
      ID = !!ID,
      y0 = !!y0,
      y1 = !!y1,
      y2 = !!y2
    ) %>%
    dplyr::mutate(
      ID = as.factor(ID),
      y0 = as.numeric(y0),
      y1 = as.numeric(y1),
      y2 = as.numeric(y2)
    )

  # coerce extra vars to factors if they exist
  if (!is.null(extra_vars)) {
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(extra_vars),
          .fns  = ~ as.factor(.x)
        )
      )
  }

  # --- simple sanity check -------------------------------------------------
  if (any(is.na(df$y0) | is.na(df$y1) | is.na(df$y2))) {
    rlang::warn(
      "Some y0/y1/y2 values could not be coerced to numeric and became NA.",
      class = "srtm_import_na_values"
    )
  }

  df
}
