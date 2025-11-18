compareOutcomes <- function(data,
                            obs        = y2,
                            exp        = exp_y2,
                            baseGroups = baseGroup,
                            trajGroups = trajGroup) {

  # --- basic checks --------------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_compare_bad_data"
    )
  }

  # resolve column names from quosures
  obs_name  <- resolve_name(rlang::enquo(obs),        arg = "obs")
  exp_name  <- resolve_name(rlang::enquo(exp),        arg = "exp")
  base_name <- resolve_name(rlang::enquo(baseGroups), arg = "baseGroups")
  traj_name <- resolve_name(rlang::enquo(trajGroups), arg = "trajGroups")

  required_cols <- c(obs_name, exp_name, base_name, traj_name)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue(
        "Missing required columns: {paste(missing_cols, collapse = ', ')}."
      ),
      class = "srtm_compare_missing_cols"
    )
  }

  # numeric checks for obs / exp
  if (!is.numeric(data[[obs_name]])) {
    rlang::abort(
      glue::glue("Column `{obs_name}` must be numeric."),
      class = "srtm_compare_non_numeric"
    )
  }

  if (!is.numeric(data[[exp_name]])) {
    rlang::abort(
      glue::glue("Column `{exp_name}` must be numeric."),
      class = "srtm_compare_non_numeric"
    )
  }

  df <- tibble::as_tibble(data)

  # ensure grouping vars are factors (for nicer output)
  if (!is.factor(df[[base_name]])) {
    df[[base_name]] <- factor(df[[base_name]])
  }
  if (!is.factor(df[[traj_name]])) {
    df[[traj_name]] <- factor(df[[traj_name]])
  }

  # --- per-group paired comparisons via obs - exp --------------------------
  out <- df %>%
    dplyr::group_by(
      .data[[base_name]],
      .data[[traj_name]]
    ) %>%
    dplyr::group_modify(function(.x, .g) {

      diffs <- .x[[obs_name]] - .x[[exp_name]]
      diffs <- diffs[is.finite(diffs)]

      n_diff <- length(diffs)

      # too few observations -> no test
      if (n_diff < 2L) {
        return(tibble::tibble(
          t_value  = NA_real_,
          diff     = NA_real_,
          p        = NA_real_,
          pretty_p = NA_character_,
          df       = NA_real_
        ))
      }

      # constant differences: mean well-defined, t-test undefined
      if (length(unique(diffs)) < 2L) {
        mean_diff <- mean(diffs)

        return(tibble::tibble(
          t_value  = NA_real_,
          diff     = mean_diff,
          p        = NA_real_,
          pretty_p = NA_character_,
          df       = n_diff - 1
        ))
      }

      tt   <- stats::t.test(diffs, mu = 0)
      pval <- tt$p.value

      pretty_p <- if (is.na(pval)) {
        NA_character_
      } else if (pval < 1e-4) {
        "<.0001"
      } else {
        formatC(pval, digits = 3, format = "fg")
      }

      tibble::tibble(
        t_value  = unname(tt$statistic),
        diff     = unname(tt$estimate),   # mean(obs - exp)
        p        = pval,
        pretty_p = pretty_p,
        df       = unname(tt$parameter)
      )
    }) %>%
    dplyr::ungroup()

  # standardise column order; the grouping columns keep their original names
  out %>%
    dplyr::select(
      !!base_name,
      !!traj_name,
      t_value,
      diff,
      p,
      pretty_p,
      df
    )
}
