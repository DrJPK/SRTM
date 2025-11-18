SRTMAnalyse <- function(data,
                        y0          = "y0",
                        y1          = "y1",
                        y2          = "y2",
                        id_col      = "ID",
                        time01      = NULL,
                        time12      = NULL,
                        group_first = c("slope", "baseline"),
                        interactive = TRUE) {

  # reset time state for this analysis run
  srtm_reset_time_state()

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_analyse_bad_data"
    )
  }

  group_first <- rlang::arg_match(group_first)

  rlang::inform(
    glue::glue("SRTMAnalyse: grouping first by `{group_first}`."),
    class = "srtm_analyse_progress"
  )

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

  # --- handle ID column ----------------------------------------------------
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

  # store all findGroups() outputs for later plotting
  group_params_store <- list(
    baseline_overall = NULL,  # findGroups on y1 for whole sample
    traj_overall     = NULL,  # findGroups on m01 for whole sample
    traj_by_base     = list(),# findGroups on m01 within each baseGroup
    baseline_by_traj = list() # findGroups on y1 within each trajGroup
  )

  # --- obtain time periods -------------------------------------------------
  dt01 <- getTimePeriod(
    time_period = time01,
    interactive = interactive,
    label       = glue::glue("{y0_name} and {y1_name}")
  )

  dt12 <- getTimePeriod(
    time_period = time12,
    interactive = interactive,
    label       = glue::glue("{y1_name} and {y2_name}")
  )

  df$dt01 <- dt01
  df$dt12 <- dt12

  # Also create t0, t1, t2 depending on mode -------------------------------
  if (identical(.srtm_time_state$mode, "dates") &&
      !is.null(.srtm_time_state$t0) &&
      !is.null(.srtm_time_state$t1) &&
      !is.null(.srtm_time_state$t2)) {

    df$t0 <- .srtm_time_state$t0
    df$t1 <- .srtm_time_state$t1
    df$t2 <- .srtm_time_state$t2

  } else {
    # duration mode or user-supplied numeric times:
    # by convention: y1 at 0, y0 at -dt01, y2 at +dt12
    df$t0 <- -dt01
    df$t1 <- 0
    df$t2 <- dt12
  }

  rlang::inform(
    glue::glue("SRTMAnalyse: using dt01 = {dt01}, dt12 = {dt12}."),
    class = "srtm_analyse_progress"
  )

  # --- calculate m01 (slope between y0 and y1) -----------------------------
  rlang::inform(
    glue::glue("SRTMAnalyse: calculating m01 from `{y0_name}` to `{y1_name}`."),
    class = "srtm_analyse_progress"
  )

  df$m01 <- calculateTrajectories(
    data        = df,
    y0          = y0_name,
    y1          = y1_name,
    time_period = dt01,
    interactive = FALSE
  )

  # ========================================================================
  # GROUPING LOGIC
  # ========================================================================
  if (group_first == "baseline") {
    # 1) Base groups on y1
    # 2) Within each baseGroup, trajectory groups on m01

    rlang::inform(
      glue::glue("SRTMAnalyse: finding baseline groups on `{y1_name}`."),
      class = "srtm_analyse_progress"
    )

    base_params <- findGroups(
      data        = df,
      time_var    = y1_name,
      interactive = interactive,
      show_plot   = interactive
    )

    group_params_store$baseline_overall <- base_params

    rlang::inform(
      glue::glue("SRTMAnalyse: assigning baseGroup using assignGroups() on `{y1_name}`."),
      class = "srtm_analyse_progress"
    )

    df$baseGroup <- assignGroups(
      data         = df,
      group_params = base_params,
      interactive  = interactive
    )
    df$baseGroup <- factor(as.character(df$baseGroup), ordered = TRUE)

    rlang::inform(
      "SRTMAnalyse: now finding trajectory groups (trajGroup) within each baseGroup using `m01`.",
      class = "srtm_analyse_progress"
    )

    traj_params_by_base <- list()

    df <- df %>%
      dplyr::group_by(baseGroup) %>%
      dplyr::group_modify(function(.x, .g) {
        current_bg <- as.character(.g$baseGroup[[1]])
        n_rows     <- nrow(.x)
        n_nonmiss  <- sum(!is.na(.x$m01))

        rlang::inform(
          glue::glue(
            "  Analysing trajGroups within baseGroup = '{current_bg}' (n = {n_rows}, non-missing m01 = {n_nonmiss})."
          ),
          class = "srtm_analyse_progress"
        )

        if (n_nonmiss < 5L) {
          rlang::inform(
            glue::glue(
              "  Too few non-missing m01 values in baseGroup '{current_bg}' (non-missing = {n_nonmiss} < 5). Assigning single trajGroup 'Unknown'."
            ),
            class = "srtm_analyse_traj_fallback"
          )
          .x$trajGroup <- factor("Unknown", levels = "Unknown", ordered = TRUE)
          return(.x)
        }

        rlang::inform(
          glue::glue(
            "  Calling findGroups() for trajGroups in baseGroup '{current_bg}' on 'm01'."
          ),
          class = "srtm_analyse_progress"
        )

        traj_params <- findGroups(
          data        = .x,
          time_var    = "m01",
          interactive = interactive,
          show_plot   = interactive
        )

        traj_params_by_base[[current_bg]] <<- traj_params

        rlang::inform(
          glue::glue(
            "  Calling assignGroups() for trajGroups in baseGroup '{current_bg}' (label_scheme = 'arrows')."
          ),
          class = "srtm_analyse_progress"
        )

        .x$trajGroup <- assignGroups(
          data         = .x,
          group_params = traj_params,
          interactive  = interactive,
          label_scheme = "arrows"
        )

        # return as *character* to avoid incompatible ordered factors
        .x$trajGroup <- as.character(.x$trajGroup)
        .x
      }) %>%
      dplyr::ungroup()

    # now standardise trajGroup globally as an ordered factor
    df$trajGroup <- factor(
      df$trajGroup,
      levels  = sort(unique(df$trajGroup)),
      ordered = TRUE
    )

    group_params_store$traj_by_base <- traj_params_by_base

  } else { # group_first == "slope"
    # 1) Trajectory groups on m01
    # 2) Within each trajGroup, baseline groups on y1

    rlang::inform(
      "SRTMAnalyse: finding trajectory groups (trajGroup) on `m01` for the whole sample.",
      class = "srtm_analyse_progress"
    )

    traj_params_all <- findGroups(
      data        = df,
      time_var    = "m01",
      interactive = interactive,
      show_plot   = interactive
    )

    group_params_store$traj_overall <- traj_params_all

    rlang::inform(
      "SRTMAnalyse: assigning trajGroup using assignGroups() with label_scheme = 'arrows'.",
      class = "srtm_analyse_progress"
    )

    df$trajGroup <- assignGroups(
      data         = df,
      group_params = traj_params_all,
      interactive  = interactive
    )
    df$trajGroup <- factor(as.character(df$trajGroup), ordered = TRUE)

    rlang::inform(
      "SRTMAnalyse: now finding baseline groups (baseGroup) within each trajGroup using `y1`.",
      class = "srtm_analyse_progress"
    )

    base_params_by_traj <- list()

    df <- df %>%
      dplyr::group_by(trajGroup) %>%
      dplyr::group_modify(function(.x, .g) {
        current_tg <- as.character(.g$trajGroup[[1]])
        n_rows     <- nrow(.x)
        n_nonmiss  <- sum(!is.na(.x[[y1_name]]))

        rlang::inform(
          glue::glue(
            "  Analysing baseGroups within trajGroup = '{current_tg}' (n = {n_rows}, non-missing {y1_name} = {n_nonmiss})."
          ),
          class = "srtm_analyse_progress"
        )

        if (n_nonmiss < 5L) {
          rlang::inform(
            glue::glue(
              "  Too few non-missing {y1_name} values in trajGroup '{current_tg}' (non-missing = {n_nonmiss} < 5). Assigning single baseGroup 'Unknown'."
            ),
            class = "srtm_analyse_base_fallback"
          )
          .x$baseGroup <- factor("Unknown", levels = "Unknown", ordered = TRUE)
          return(.x)
        }

        rlang::inform(
          glue::glue(
            "  Calling findGroups() for baseGroups in trajGroup '{current_tg}' on `{y1_name}`."
          ),
          class = "srtm_analyse_progress"
        )

        base_params <- findGroups(
          data        = .x,
          time_var    = y1_name,
          interactive = interactive,
          show_plot   = interactive
        )

        base_params_by_traj[[current_tg]] <<- base_params

        rlang::inform(
          glue::glue(
            "  Calling assignGroups() for baseGroups in trajGroup '{current_tg}'."
          ),
          class = "srtm_analyse_progress"
        )

        .x$baseGroup <- assignGroups(
          data         = .x,
          group_params = base_params,
          interactive  = interactive
        )

        .x$baseGroup <- as.character(.x$baseGroup)
        .x
      }) %>%
      dplyr::ungroup()

    df$baseGroup <- factor(
      df$baseGroup,
      levels  = sort(unique(df$baseGroup)),
      ordered = TRUE
    )

    group_params_store$baseline_by_traj <- base_params_by_traj
  }

  # after trajGroup has been assigned compute labels
  df$trajType <- srtm_compute_trajType(
    data         = df,
    slope_col    = "m01",
    group_col    = "trajGroup",
    label_scheme = "arrows",  # or "signs"/"text" later
    dt01         = dt01
  )

  # --- fit group-specific linear models and compute exp_y2 -----------------
  rlang::inform(
    "SRTMAnalyse: fitting group-specific linear models and computing exp_y2.",
    class = "srtm_analyse_progress"
  )

  df <- predictPostResponse(
    data       = df,
    y0         = y0_name,
    y1         = y1_name,
    y2         = y2_name,
    base_group = "baseGroup",
    traj_group = "trajGroup",
    time01     = dt01,
    time12     = dt12
  )

  # --- statistical comparisons ---------------------------------------------
  rlang::inform(
    "SRTMAnalyse: running compareOutcomes() for obs vs exp.",
    class = "srtm_analyse_progress"
  )

  res <- compareOutcomes(
    data       = df,
    obs        = y2_name,
    exp        = "exp_y2",
    baseGroups = "baseGroup",
    trajGroups = "trajGroup"
  )

  # --- group-level summaries -----------------------------------------------
  rlang::inform(
    "SRTMAnalyse: summarising group-level outcomes.",
    class = "srtm_analyse_progress"
  )

  group_summary <- summariseGroupOutcomes(
    data       = df,
    obs        = y2_name,
    exp        = "exp_y2",
    base_group = "baseGroup",
    traj_group = "trajGroup",
    drop       = TRUE
  )

  # --- assemble settings and pull slope thresholds -------------------------
  settings <- list(
    y0          = y0_name,
    y1          = y1_name,
    y2          = y2_name,
    id_col      = id_col,
    time01      = dt01,
    time12      = dt12,
    group_first = group_first,
    time_mode   = .srtm_time_state$mode        %||% "duration",
    time_unit   = .srtm_time_state$unit_label  %||% "Time units"
  )

  if (identical(.srtm_time_state$mode, "dates")) {
    settings$dates <- list(
      t0 = .srtm_time_state$t0,
      t1 = .srtm_time_state$t1,
      t2 = .srtm_time_state$t2
    )
  }

  # store thresholds if available
  thr_attr <- attr(df$trajType, "srtm_slope_thresholds", exact = TRUE)
  if (!is.null(thr_attr)) {
    settings$trajThresholds <- thr_attr
  }
  # --- assemble result object ----------------------------------------------
  out <- list(
    Comparisons  = res,
    GroupSummary = group_summary,
    data         = df,
    settings     = settings,
    group_params = group_params_store
  )

  class(out) <- c("srtm_analysis", class(out))
  out
}
