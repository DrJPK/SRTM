#' Resolve a quosure to a simple column name
#'
#' @description
#' Internal helper that takes a quosure (created with `rlang::enquo()`)
#' and returns a simple column name as a string.
#'
#' It supports:
#' - bare column names (e.g. `y2`)
#' - character literals (e.g. `"y2"`)
#' - symbols bound to character scalars (e.g. `y_col <- "y2"; obs = y_col`)
#'
#' More complex expressions (e.g. `df$y2`, `y0 + 1`) are not supported and
#' will raise an error.
#'
#' @keywords internal
resolve_name <- function(q, arg = NULL) {
  expr <- rlang::get_expr(q)

  if (is.null(arg)) {
    arg <- rlang::as_label(expr)
  }

  # character literal directly in the call, e.g. obs = "y2"
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }

  # bare symbol, or symbol whose value might be a character scalar
  if (rlang::is_symbol(expr)) {
    # try to evaluate; if it's a character scalar, use that
    val <- tryCatch(rlang::eval_tidy(q), error = function(e) NULL)

    if (is.character(val) && length(val) == 1L) {
      return(val)
    }

    # otherwise, treat the symbol itself as the column name
    return(rlang::as_string(expr))
  }

  # anything else is too complex for our use
  rlang::abort(
    glue::glue("`{arg}` must be a simple column name (bare or string)."),
    class = "srtm_bad_colspec"
  )
}
