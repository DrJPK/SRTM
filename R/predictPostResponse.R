#' Predict post measurement using group-specific linear trajectories
#'
#' @description
#' Fits simple linear models within each combination of baseline group and
#' trajectory group using `y0` and `y1`, then uses the resulting group-specific
#' slopes to generate an expected post score at the time of `y2`.
#'
#' @param data A data frame or tibble containing the specified columns.
#' @param y0,y1,y2 Column specifications (names or tidy-select) for the three
#'   time points.
#' @param base_group Column specification for the baseline grouping variable
#'   (e.g. `"baseGroup"`).
#' @param traj_group Column specification for the trajectory grouping variable
#'   (e.g. `"trajGroup"`).
#' @param time01 Numeric scalar giving the elapsed time between `y0` and `y1`.
#' @param time12 Numeric scalar giving the elapsed time between `y1` and `y2`.
#'
#' @return
#' The input `data` with additional numeric columns:
#' \itemize{
#'   \item `intercept01` — intercept from the group-specific model `y ~ t`.
#'   \item `slope01` — slope from the group-specific model between `y0` and `y1`.
#'   \item `exp_y2` — expected value at the time of `y2`, computed as
#'         `y1 + slope01 * time12`.
#' }
#'
#' @export
predictPostResponse <- function(data,
                                y0         = "y0",
                                y1         = "y1",
                                y2         = "y2",
                                base_group = "baseGroup",
                                traj_group = "trajGroup",
                                time01,
                                time12) {

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_predict_bad_data"
    )
  }

  # resolve column names (support bare names or character)
  resolve_name <- function(x) {
    if (is.character(x) && length(x) == 1L) {
      x
    } else {
      rlang::as_string(rlang::ensym(x))
    }
  }

  y0_name        <- resolve_name(y0)
  y1_name        <- resolve_name(y1)
  y2_name        <- resolve_name(y2)
  base_group_col <- resolve_name(base_group)
  traj_group_col <- resolve_name(traj_group)

  required_cols <- c(y0_name, y1_name, y2_name, base_group_col, traj_group_col)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue(
        "Missing required columns: {paste(missing_cols, collapse = ', ')}."
      ),
      class = "srtm_predict_missing_cols"
    )
  }

  # simple numeric checks on times
  for (nm in c("time01", "time12")) {
    val <- get(nm)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val) || val <= 0) {
      rlang::abort(
        glue::glue("`{nm}` must be a single positive numeric value."),
        class = "srtm_predict_bad_time"
      )
    }
  }

  df <- tibble::as_tibble(data)

  # fit per (baseGroup, trajGroup) models and collect intercept/slope
  coefs <- df %>%
    dplyr::group_by(
      .data[[base_group_col]],
      .data[[traj_group_col]]
    ) %>%
    dplyr::group_modify(function(.x, .g) {

      ok <- !is.na(.x[[y0_name]]) & !is.na(.x[[y1_name]])
      if (sum(ok) < 2L) {
        return(tibble::tibble(
          intercept01 = NA_real_,
          slope01     = NA_real_
        ))
      }

      long_df <- .x[ok, c(y0_name, y1_name)] %>%
        tidyr::pivot_longer(
          cols      = tidyselect::all_of(c(y0_name, y1_name)),
          names_to  = "tp",
          values_to = "y"
        ) %>%
        dplyr::mutate(
          t = dplyr::case_when(
            tp == y1_name ~ 0,
            tp == y0_name ~ -time01,
            TRUE ~ NA_real_
          )
        ) %>%
        dplyr::filter(!is.na(t), !is.na(y))

      if (nrow(long_df) < 2L || length(unique(long_df$t)) < 2L) {
        return(tibble::tibble(
          intercept01 = NA_real_,
          slope01     = NA_real_
        ))
      }

      mod <- stats::lm(y ~ t, data = long_df)
      cf  <- stats::coef(mod)

      intercept01 <- unname(cf["(Intercept)"])
      slope01     <- unname(cf["t"])

      tibble::tibble(
        intercept01 = intercept01,
        slope01     = slope01
      )
    }) %>%
    dplyr::ungroup()

  # join coefs back and compute exp_y2
  df %>%
    dplyr::left_join(
      coefs,
      by = setNames(
        c(base_group_col, traj_group_col),
        c(base_group_col, traj_group_col)
      )
    ) %>%
    dplyr::mutate(
      exp_y2 = .data[[y1_name]] + slope01 * time12
    )
}
