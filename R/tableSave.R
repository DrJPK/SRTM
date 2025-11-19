#' Save an SRTM table in a journal- or presentation-ready format
#'
#' @description
#' Convenience helper for turning SRTM summary/comparison tables into
#' publication-ready output. Typical inputs are:
#' \itemize{
#'   \item `results$Comparisons` from [SRTMAnalyse()], or
#'   \item `report_obj$table` from [report()].
#' }
#'
#' The function will:
#' \itemize{
#'   \item merge `baseGroup` and `trajType`/`trajGroup` into a
#'         `"Baseline × Trajectory"` column;
#'   \item replace raw columns with their `pretty_` versions when present
#'         (e.g., `pretty_p` replaces `p`);
#'   \item rename and reorder common columns (e.g. `t_value` → `t`);
#'   \item format numeric columns sensibly (e.g. 2–3 decimal places).
#' }
#'
#' Output depends on `format`:
#' \itemize{
#'   \item `"journal"` – saves a `.docx` and `.rtf` using
#'         **officer** + **flextable**;
#'   \item `"presentation"` – saves a `.png` image of the table using
#'         [flextable::save_as_image()].
#' }
#'
#' @param x A data frame or tibble, typically `results$Comparisons` or
#'   `report_obj$table`.
#' @param file_stem Character stem for the output filename (no extension).
#' @param dir Directory to save into. Defaults to `"outputs"`.
#' @param format One of `"journal"` or `"presentation"`.
#' @param table_type One of `"auto"`, `"comparisons"`, or `"summary"`.
#'   If `"auto"` (default), the function inspects `x`:
#'   presence of `t_value`/`diff`/`p` implies `"comparisons"`, otherwise
#'   `"summary"` if `Mean`/`SD` present.
#' @param ... Reserved for future arguments.
#'
#' @return Invisibly returns a character vector of file paths that were
#'   written.
#'
#' @export
tableSave <- function(x,
                      file_stem,
                      dir        = "outputs",
                      format     = c("journal", "presentation"),
                      table_type = c("auto", "comparisons", "summary"),
                      ...) {

  # small internal helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (missing(file_stem) || !nzchar(file_stem)) {
    rlang::abort("`file_stem` must be a non-empty character string.",
                 class = "srtm_tableSave_bad_filestem")
  }

  format     <- rlang::arg_match(format)
  table_type <- rlang::arg_match(table_type)

  # Coerce to tibble
  df <- tibble::as_tibble(x)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # --------------------------------------------------------------------------
  # 1) Infer table type if needed
  # --------------------------------------------------------------------------
  if (identical(table_type, "auto")) {
    nms <- names(df)
    if (any(c("t_value", "diff", "p", "pretty_p") %in% nms)) {
      table_type <- "comparisons"
    } else if (any(c("Mean", "SD") %in% nms)) {
      table_type <- "summary"
    } else {
      table_type <- "summary"  # safe-ish fallback
    }
  }

  # --------------------------------------------------------------------------
  # 2) Tidy: baseGroup + trajType/trajGroup, pretty_ columns, renaming
  # --------------------------------------------------------------------------
  df <- .srtm_tidy_table_core(df, table_type = table_type)

  # --------------------------------------------------------------------------
  # 3) Build a flextable object for pretty output
  # --------------------------------------------------------------------------
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    rlang::abort(
      "Packages 'flextable' and 'officer' are required for tableSave().\n",
      "Please install them with install.packages(c('flextable', 'officer')).",
      class = "srtm_tableSave_missing_deps"
    )
  }

  ft <- flextable::flextable(df)

  # Basic formatting: auto-fit, header bold, a little padding
  ft <- flextable::autofit(ft)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, part = "all", size = 9)
  ft <- flextable::bold(ft, part = "header")

  # Numeric formatting: separate integer vs numeric
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) > 0L) {
    int_cols  <- num_cols[vapply(df[num_cols], function(z) all(z %% 1 == 0, na.rm = TRUE), logical(1))]
    real_cols <- setdiff(num_cols, int_cols)

    if (length(int_cols) > 0L) {
      ft <- flextable::colformat_int(ft, j = int_cols, big.mark = ",")
    }
    if (length(real_cols) > 0L) {
      # 2–3 dp for real-valued columns
      ft <- flextable::colformat_num(ft, j = real_cols, digits = 3)
    }
  }

  saved_files <- character(0)

  # --------------------------------------------------------------------------
  # 4) Output by format
  # --------------------------------------------------------------------------
  if (identical(format, "journal")) {
    # DOCX
    docx_file <- file.path(dir, paste0(file_stem, "_journal.docx"))
    doc       <- officer::read_docx()
    doc       <- officer::body_add_flextable(doc, ft)
    print(doc, target = docx_file)
    saved_files <- c(saved_files, docx_file)

    # RTF
    rtf_file <- file.path(dir, paste0(file_stem, "_journal.rtf"))
    rdoc     <- officer::read_rtf()
    rdoc     <- officer::body_add_flextable(rdoc, ft)
    print(rdoc, target = rtf_file)
    saved_files <- c(saved_files, rtf_file)

  } else if (identical(format, "presentation")) {
    # PNG image for slides
    if (!requireNamespace("webshot2", quietly = TRUE) &&
        !requireNamespace("webshot", quietly = TRUE)) {
      rlang::warn(
        "Saving as image requires either 'webshot2' or 'webshot' to be installed.\n",
        class = "srtm_tableSave_no_webshot"
      )
    }

    png_file <- file.path(dir, paste0(file_stem, "_presentation.png"))
    flextable::save_as_image(
      x    = ft,
      path = png_file
    )
    saved_files <- c(saved_files, png_file)
  }

  invisible(saved_files)
}

# ---------------------------------------------------------------------------
# Internal helper to tidy SRTM tables
# ---------------------------------------------------------------------------
.srtm_tidy_table_core <- function(df, table_type = c("comparisons", "summary")) {
  table_type <- match.arg(table_type)

  # 1) Merge baseGroup + trajType/trajGroup into "Baseline × Trajectory"
  has_base <- "baseGroup"  %in% names(df)
  has_ttype <- "trajType"  %in% names(df)
  has_tgrp  <- "trajGroup" %in% names(df)

  if (has_base && (has_ttype || has_tgrp)) {
    traj_col <- if (has_ttype) "trajType" else "trajGroup"

    df <- df %>%
      dplyr::mutate(
        `Baseline × Trajectory` = paste0(
          as.character(.data$baseGroup),
          as.character(.data[[traj_col]])
        )
      ) %>%
      dplyr::relocate(`Baseline × Trajectory`, .before = 1) %>%
      dplyr::select(-dplyr::all_of(c("baseGroup", traj_col)))
  }

  # 2) Replace raw cols with pretty_ versions where present
  nms <- names(df)
  pretty_cols <- grep("^pretty_", nms, value = TRUE)

  for (pc in pretty_cols) {
    base <- sub("^pretty_", "", pc)
    if (base %in% nms) {
      df[[base]] <- df[[pc]]
    }
  }
  # Drop pretty_ columns after replacement
  df <- df[, !grepl("^pretty_", names(df)), drop = FALSE]

  # 3) Rename common columns & reorder
  if (identical(table_type, "comparisons")) {
    rename_map <- c(
      t_value = "t",
      diff    = "Mean difference"
    )
    # rename if present
    for (nm in names(rename_map)) {
      if (nm %in% names(df)) {
        df <- dplyr::rename(df, !!rename_map[[nm]] := .data[[nm]])
      }
    }

    # Preferred column order
    order_cols <- c(
      "Baseline × Trajectory",
      "n",
      "Mean",
      "SD",
      "Slope",
      "Mean difference",
      "t",
      "df",
      "p"
    )
  } else {
    # summary table
    rename_map <- c(
      Slope = "Slope (Δy/Δt)"
    )
    for (nm in names(rename_map)) {
      if (nm %in% names(df)) {
        df <- dplyr::rename(df, !!rename_map[[nm]] := .data[[nm]])
      }
    }

    order_cols <- c(
      "Baseline × Trajectory",
      "n",
      "Mean",
      "SD",
      "Slope (Δy/Δt)"
    )
  }

  # Reorder columns where possible
  present_order <- order_cols[order_cols %in% names(df)]
  remaining     <- setdiff(names(df), present_order)
  df <- df[, c(present_order, remaining), drop = FALSE]

  df
}
