SRTMAnalyse <- function(data,
                        y0          = "y0",
                        y1          = "y1",
                        y2          = "y2",
                        id_col      = "ID",
                        time01      = NULL,
                        time12      = NULL,
                        interactive = TRUE) {

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_analyse_bad_data"
    )
  }

  # turn user-supplied arguments into *column names*
  y0_name <- rlang::as_string(rlang::ensym(y0))
  y1_name <- rlang::as_string(rlang::ensym(y1))
  y2_name <- rlang::as_string(rlang::ensym(y2))

  required_cols <- c(y0_name, y1_name, y2_name)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      glue::glue(
        "Missing required numeric columns: {paste(missing_cols, collapse = ', ')}."
      ),
      class = "srtm_analyse_missing_cols"
    )
  }

  for (nm in required_cols) {
    if (!is.numeric(data[[nm]])) {
      rlang::abort(
        glue::glue("Column `{nm}` must be numeric."),
        class = "srtm_analyse_non_numeric"
      )
    }
  }

  df <- tibble::as_tibble(data)

  # --- handle ID column if not supplied
  if (id_col %in% names(df)) {
    df[[id_col]] <- as.factor(df[[id_col]])
  } else {
    id_vec <- sprintf("ID%06d", seq_len(nrow(df)))
    df[[id_col]] <- factor(id_vec, levels = id_vec)
    rlang::inform(
      glue::glue(
        "No `{id_col}` column found. Generated synthetic IDs of the form ID000001…ID{sprintf('%06d', nrow(df))}."
      ),
      class = "srtm_analyse_generated_id"
    )
  }

  # --- obtain time periods
  # time between y0 and y1
  dt01 <- getTimePeriod(
    time_period = time01,
    interactive = interactive,
    label       = glue::glue("{y0_name} and {y1_name}")
  )

  # time between y1 and y2
  dt12 <- getTimePeriod(
    time_period = time12,
    interactive = interactive,
    label       = glue::glue("{y1_name} and {y2_name}")
  )

  # store for later plotting/use
  df$dt01 <- dt01
  df$dt12 <- dt12

  # --- calculate m01 (slope between y0 and y1) -----------------------------
  df$m01 <- calculateTrajectories(
    data        = df,
    y0          = y0_name,
    y1          = y1_name,
    time_period = dt01,
    interactive = FALSE
  )

  # --- find baseline groups on y1 ------------------------------------------
  base_params <- findGroups(
    data        = df,
    time_var    = y1_name,
    interactive = interactive,
    show_plot   = interactive
  )

  df$baseGroup <- assignGroups(
    data         = df,
    group_params = base_params,
    interactive  = interactive
  )
  df$baseGroup <- as.factor(df$baseGroup)

  # --- within each baseGroup, find trajectory groups on m01 ----------------
  df <- df %>%
    dplyr::group_by(baseGroup) %>%
    dplyr::group_modify(function(.x, .g) {
      if (sum(!is.na(.x$m01)) < 5L) {
        .x$trajGroup <- factor("A", levels = "A")
        return(.x)
      }

      traj_params <- findGroups(
        data        = .x,
        time_var    = "m01",
        interactive = interactive,
        show_plot   = FALSE
      )

      .x$trajGroup <- assignGroups(
        data         = .x,
        group_params = traj_params,
        interactive  = interactive
      )

      .x$trajGroup <- factor(as.character(.x$trajGroup))

      .x
    }) %>%
    dplyr::ungroup()

  # --- fit group-specific linear models to get y2_exp ----------------------
  coefs <- df %>%
    dplyr::group_by(baseGroup, trajGroup) %>%
    dplyr::group_modify(function(.x, .g) {
      # rows with both y0 and y1 present
      ok <- !is.na(.x[[y0_name]]) & !is.na(.x[[y1_name]])
      if (sum(ok) < 2L) {
        return(tibble::tibble(
          intercept01 = NA_real_,
          slope01     = NA_real_
        ))
      }

      long_df <- .x[ok, c(id_col, y0_name, y1_name)] %>%
        tidyr::pivot_longer(
          cols      = tidyselect::all_of(c(y0_name, y1_name)),
          names_to  = "tp",
          values_to = "y"
        ) %>%
        dplyr::mutate(
          t = dplyr::case_when(
            tp == y1_name ~ 0,
            tp == y0_name ~ -dt01,
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

  # --- join group slopes back and compute exp_y2 ---------------------------
  df <- df %>%
    dplyr::left_join(coefs, by = c("baseGroup", "trajGroup")) %>%
    dplyr::mutate(
      exp_y2 = .data[[y1_name]] + slope01 * dt12
    )

  df
}
