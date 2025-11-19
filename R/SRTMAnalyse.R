#' Run a full Self-Referenced Trajectory Modelling (SRTM) analysis
#'
#' @description
#' `SRTMAnalyse()` is the main entry point for the SRTM package. Given repeated
#' measures at three occasions (y0, y1, y2), it:
#' \itemize{
#'   \item sets up the time scale between measurements (from supplied arguments
#'         or time-related columns),
#'   \item computes individual slopes between y0–y1 and y1–y2,
#'   \item forms baseline and trajectory groups via \code{\link{findGroups}()}
#'         and \code{\link{assignGroups}()},
#'   \item fits group-specific linear models to predict post scores at y2,
#'   \item compares observed and expected outcomes via
#'         \code{\link{compareOutcomes}()} and, optionally,
#'         \code{\link{compareSlopes}()},
#'   \item and summarises group-level patterns via
#'         \code{\link{summariseGroupOutcomes}()}.
#' }
#'
#' The result is a structured object of class \code{"srtm_analysis"} containing:
#' \itemize{
#'   \item the augmented analysis data (IDs, time variables, slopes, groups,
#'         expected outcomes, etc.),
#'   \item group-level comparisons of observed vs expected outcomes and slopes,
#'   \item compact summaries for each baseline × trajectory type,
#'   \item and the settings and grouping parameters used to construct the model.
#' }
#'
#' @section Time handling:
#'
#' `SRTMAnalyse()` can obtain the time intervals between occasions in several
#' ways. The priority order is:
#'
#' \enumerate{
#'   \item \strong{Explicit arguments}:
#'     \itemize{
#'       \item If \code{time01} and \code{time12} are supplied as positive
#'             numerics, they are used directly (after validation).
#'     }
#'   \item \strong{Numeric time-interval columns}:
#'     \itemize{
#'       \item If the data contain numeric columns named \code{time01} and
#'             \code{time12}, their (median) values are used as dt01 and dt12.
#'     }
#'   \item \strong{Absolute time columns t0, t1, t2}:
#'     \itemize{
#'       \item If columns corresponding to \code{t0}, \code{t1}, \code{t2} are
#'             present (either with the default names \code{"t0"}, \code{"t1"},
#'             \code{"t2"} or user-specified names), the function attempts to:
#'             \itemize{
#'               \item interpret them as dates (using \pkg{lubridate} when
#'                     needed) and derive dt01 and dt12 from their differences,
#'                     choosing a sensible time unit (years, months, weeks, or
#'                     days), or
#'               \item interpret them as numeric times with \code{t0 < t1 < t2}
#'                     and set \code{dt01 = t1 - t0}, \code{dt12 = t2 - t1},
#'                     asking only for a time-unit label.
#'             }
#'     }
#'   \item \strong{Interactive fallback}:
#'     \itemize{
#'       \item If none of the above sources provide valid time intervals and
#'             \code{interactive = TRUE}, the function calls
#'             \code{\link{getTimePeriod}()} to obtain dt01 and dt12 (and a time
#'             unit label) interactively.
#'       \item If \code{interactive = FALSE} and time intervals cannot be
#'             determined, an error is thrown.
#'     }
#' }
#'
#' Internally, numeric time intervals are stored as \code{dt01} and \code{dt12},
#' and either date-based (\code{t0}, \code{t1}, \code{t2}) or centred numeric
#' time variables are added to the analysis data as appropriate.
#'
#' @param data A data frame or tibble containing at least three numeric columns
#'   representing repeated measurements at three time points. These columns are
#'   identified by \code{y0}, \code{y1}, and \code{y2}. The data may also
#'   contain ID and time-related columns.
#'
#' @param y0,y1,y2 Variables representing the outcome at the three measurement
#'   occasions used in the SRTM workflow. By default these are the character
#'   strings \code{"y0"}, \code{"y1"}, and \code{"y2"}, but they can also be
#'   supplied as unquoted column names (e.g., \code{y0 = baseline_score}).
#'
#' @param id_col Character string giving the name of the ID column in
#'   \code{data}. If present, it is coerced to a factor. If absent, synthetic
#'   IDs of the form \code{"ID000001"}, \code{"ID000002"}, … are generated and
#'   an informational message is emitted.
#'
#' @param t0,t1,t2 Optional variables naming columns in \code{data} that store
#'   the absolute times of the three occasions. These can be unquoted column
#'   names or character strings. If \code{NULL}, the function looks for columns
#'   literally named \code{"t0"}, \code{"t1"}, and \code{"t2"}. When available,
#'   they are used to infer dt01 and dt12 as described in the Time handling
#'   section.
#'
#' @param time01,time12 Optional numeric time intervals between the three
#'   occasions:
#'   \itemize{
#'     \item \code{time01}: time difference between \code{y0} and \code{y1},
#'     \item \code{time12}: time difference between \code{y1} and \code{y2}.
#'   }
#'   When supplied, these override any intervals inferred from columns in
#'   \code{data}. When \code{NULL}, the function attempts to infer intervals
#'   from \code{time01}/\code{time12} columns or \code{t0}/\code{t1}/\code{t2},
#'   and finally, if \code{interactive = TRUE}, via
#'   \code{\link{getTimePeriod}()}.
#'
#' @param group_first Character string indicating the order in which groups
#'   should be formed. Must be one of:
#'   \itemize{
#'     \item \code{"baseline"}: form baseline groups on \code{y1} first, then
#'       trajectory groups on the slope \code{m01} within each baseline group;
#'     \item \code{"slope"}: form trajectory groups on \code{m01} first, then
#'       baseline groups on \code{y1} within each trajectory group.
#'   }
#'
#' @param traj_label_scheme Character string specifying the labelling scheme
#'   for trajectory groups and \code{trajType}. One of:
#'   \itemize{
#'     \item \code{"arrows"}: arrow glyphs (e.g., \code{"↟"}, \code{"↑"},
#'       \code{"→"}, \code{"↓"}, \code{"↡"}),
#'     \item \code{"signs"}: sign-like labels (e.g., \code{"++"}, \code{"+"},
#'       \code{"—"}, \code{"-"}, \code{"--"}),
#'     \item \code{"text"}: textual labels corresponding to internal rank
#'       levels (e.g., \code{"steep_pos"}, \code{"shallow_pos"}, \code{"flat"},
#'       \code{"shallow_neg"}, \code{"steep_neg"}).
#'   }
#'   Baseline groups (\code{baseGroup}) are always labelled with letters
#'   (\code{"A"}, \code{"B"}, …); the letter scheme is never used for
#'   trajectory groups.
#'
#' @param interactive Logical. If \code{TRUE} (default), the function is
#'   allowed to use interactive helpers such as \code{\link{getTimePeriod}()}
#'   and \code{\link{findGroups}()}. If \code{FALSE}, all required time
#'   intervals and grouping decisions must be available from arguments and/or
#'   columns in \code{data}.
#'
#' @return
#' An object of class \code{"srtm_analysis"}, which is a named list with
#' components including (but not limited to):
#' \describe{
#'   \item{Comparisons}{A tibble from \code{\link{compareOutcomes}()},
#'     containing one row per baseline × trajectory type with t-tests of
#'     observed vs expected outcomes at \code{y2}.}
#'   \item{Slope_Comparisons}{A tibble from \code{\link{compareSlopes}()}
#'     (where fitted), comparing slopes between intervals.}
#'   \item{GroupSummary}{A tibble from \code{\link{summariseGroupOutcomes}()},
#'     providing descriptive statistics (e.g., group sizes, means, SDs, expected
#'     values) for each baseline × trajectory type.}
#'   \item{data}{The original data augmented with additional columns used in
#'     the analysis (IDs, time variables, slopes, groups, expected outcomes,
#'     etc.).}
#'   \item{settings}{A list of analysis settings (column names, time intervals,
#'     grouping order, trajectory label scheme, time mode/unit, and any stored
#'     slope thresholds).}
#'   \item{group_params}{A list of \code{"srtm_group_suggestion"} objects from
#'     \code{\link{findGroups}()}, describing the baseline and trajectory
#'     groupings used.}
#' }
#'
#' The object can be inspected directly, passed to \code{\link{summary}} for a
#' tabular summary, or used by plotting helpers (when available).
#' @export
SRTMAnalyse <- function(data,
                        y0          = "y0",
                        y1          = "y1",
                        y2          = "y2",
                        id_col      = "ID",
                        t0          = NULL,
                        t1          = NULL,
                        t2          = NULL,
                        time01      = NULL,
                        time12      = NULL,
                        group_first = c("slope", "baseline"),
                        traj_label_scheme = c("arrows", "signs", "text"),
                        interactive = TRUE) {

  # reset time state for this analysis run
  srtm_reset_time_state()

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "srtm_analyse_bad_data"
    )
  }

  group_first       <- rlang::arg_match(group_first)
  traj_label_scheme <- rlang::arg_match(traj_label_scheme)

  rlang::inform(
    glue::glue(
      "SRTMAnalyse: grouping first by `{group_first}`, traj_label_scheme = '{traj_label_scheme}'."
    ),
    class = "srtm_analyse_progress"
  )

  # turn user-supplied arguments into *column names*
  y0_name <- rlang::as_string(rlang::ensym(y0))
  y1_name <- rlang::as_string(rlang::ensym(y1))
  y2_name <- rlang::as_string(rlang::ensym(y2))

  # optional time-column names (fall back to canonical t0/t1/t2 if NULL)
  t0_name <- if (is.null(t0)) "t0" else rlang::as_string(rlang::ensym(t0))
  t1_name <- if (is.null(t1)) "t1" else rlang::as_string(rlang::ensym(t1))
  t2_name <- if (is.null(t2)) "t2" else rlang::as_string(rlang::ensym(t2))

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
    baseline_overall = NULL,
    traj_overall     = NULL,
    traj_by_base     = list(),
    baseline_by_traj = list()
  )

  # -------------------------------------------------------------------------
  # Detect time-period information from existing columns (time01/time12/t0/t1/t2)
  # -------------------------------------------------------------------------

  time01_eff <- time01
  time12_eff <- time12
  detected_dates <- NULL

  if (is.null(time01_eff) && is.null(time12_eff)) {

    # 1) Prefer numeric `time01` and `time12` columns if present
    if (all(c("time01", "time12") %in% names(df)) &&
        is.numeric(df$time01) && is.numeric(df$time12)) {

      dt01_vals <- unique(df$time01[is.finite(df$time01)])
      dt12_vals <- unique(df$time12[is.finite(df$time12)])

      if (length(dt01_vals) > 0L && length(dt12_vals) > 0L) {
        time01_eff <- stats::median(dt01_vals)
        time12_eff <- stats::median(dt12_vals)

        rlang::inform(
          "Using numeric `time01` and `time12` columns found in `data` as dt01 and dt12.",
          class = "srtm_analyse_use_time_columns"
        )
      }
    }

    # 2) If still missing, try t0/t1/t2 (using user-provided names or defaults)
    if (is.null(time01_eff) && is.null(time12_eff) &&
        all(c(t0_name, t1_name, t2_name) %in% names(df))) {

      t0_col <- df[[t0_name]]
      t1_col <- df[[t1_name]]
      t2_col <- df[[t2_name]]

      get_first_non_missing <- function(x) x[which(!is.na(x))[1L]]

      if (any(!is.na(t0_col)) && any(!is.na(t1_col)) && any(!is.na(t2_col))) {

        t0_val <- get_first_non_missing(t0_col)
        t1_val <- get_first_non_missing(t1_col)
        t2_val <- get_first_non_missing(t2_col)

        # helper: try to coerce a value to Date (via lubridate if needed)
        coerce_date <- function(z) {
          if (inherits(z, c("Date", "POSIXt"))) {
            return(as.Date(z))
          }
          if (is.numeric(z)) {
            return(NA_Date_)  # don't treat pure numerics as dates here
          }
          tryCatch(lubridate::as_date(z), error = function(e) NA_Date_)
        }

        t0_date <- coerce_date(t0_val)
        t1_date <- coerce_date(t1_val)
        t2_date <- coerce_date(t2_val)

        if (all(!is.na(c(t0_date, t1_date, t2_date))) &&
            t0_date < t1_date && t1_date < t2_date) {

          # treat as dates; compute differences in days
          dt01_days <- as.numeric(difftime(t1_date, t0_date, units = "days"))
          dt12_days <- as.numeric(difftime(t2_date, t1_date, units = "days"))

          if (is.finite(dt01_days) && dt01_days > 0 &&
              is.finite(dt12_days) && dt12_days > 0) {

            dt_vec <- c(dt01_days, dt12_days)

            if (all(dt_vec %% 365 == 0)) {
              unit_label <- "years"
              time01_eff <- dt01_days / 365
              time12_eff <- dt12_days / 365
            } else if (all(dt_vec %% 30 == 0)) {
              unit_label <- "months"
              time01_eff <- dt01_days / 30
              time12_eff <- dt12_days / 30
            } else if (all(dt_vec %% 7 == 0)) {
              unit_label <- "weeks"
              time01_eff <- dt01_days / 7
              time12_eff <- dt12_days / 7
            } else {
              unit_label <- "days"
              time01_eff <- dt01_days
              time12_eff <- dt12_days
            }

            detected_dates <- list(
              t0         = t0_date,
              t1         = t1_date,
              t2         = t2_date,
              unit_label = unit_label
            )

            rlang::inform(
              glue::glue(
                "Using `{t0_name}`, `{t1_name}`, `{t2_name}` date columns from `data` (interpreted in {unit_label})."
              ),
              class = "srtm_analyse_use_t012_dates"
            )
          }

        } else if (is.numeric(t0_val) && is.numeric(t1_val) && is.numeric(t2_val) &&
                   t0_val < t1_val && t1_val < t2_val) {

          # treat as numeric "absolute time": derive dt01, dt12, ask only for units
          time01_eff <- t1_val - t0_val
          time12_eff <- t2_val - t1_val

          rlang::inform(
            glue::glue(
              "Using numeric `{t0_name}`, `{t1_name}`, `{t2_name}` columns from `data` to derive dt01 and dt12."
            ),
            class = "srtm_analyse_use_t012_numeric"
          )
        }
      }
    }
  }

  # --- obtain time periods (possibly using detected values) ----------------
  dt01 <- getTimePeriod(
    time_period = time01_eff,
    interactive = interactive,
    label       = glue::glue("{y0_name} and {y1_name}")
  )

  dt12 <- getTimePeriod(
    time_period = time12_eff,
    interactive = interactive,
    label       = glue::glue("{y1_name} and {y2_name}")
  )

  # If we detected date columns, record them in the time state so that
  # the "dates" branch is used when populating t0/t1/t2 below.
  if (!is.null(detected_dates)) {
    .srtm_time_state$mode       <- "dates"
    .srtm_time_state$t0         <- detected_dates$t0
    .srtm_time_state$t1         <- detected_dates$t1
    .srtm_time_state$t2         <- detected_dates$t2
    .srtm_time_state$unit_label <- detected_dates$unit_label
  }

  df$dt01 <- dt01
  df$dt12 <- dt12

  # create t0, t1, t2 depending on mode
  if (identical(.srtm_time_state$mode, "dates") &&
      !is.null(.srtm_time_state$t0) &&
      !is.null(.srtm_time_state$t1) &&
      !is.null(.srtm_time_state$t2)) {

    df$t0 <- .srtm_time_state$t0
    df$t1 <- .srtm_time_state$t1
    df$t2 <- .srtm_time_state$t2

  } else {
    df$t0 <- -dt01
    df$t1 <- 0
    df$t2 <- dt12
  }

  rlang::inform(
    glue::glue("SRTMAnalyse: using dt01 = {dt01}, dt12 = {dt12}."),
    class = "srtm_analyse_progress"
  )

  # --- calculate slopes ----------------------------------------------------
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

  df$m12 <- calculateTrajectories(
    data        = df,
    y0          = y1_name,
    y1          = y2_name,
    time_period = dt12,
    interactive = FALSE
  )

  # ========================================================================
  # GROUPING LOGIC (unchanged from your current version)
  # ========================================================================
  if (group_first == "baseline") {
    # 1) Base groups on y1 (letters)
    # 2) Within each baseGroup, trajectory groups on m01 (traj_label_scheme)

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
      glue::glue("SRTMAnalyse: assigning baseGroup using assignGroups() on `{y1_name}` (letters)."),
      class = "srtm_analyse_progress"
    )

    df$baseGroup <- assignGroups(
      data         = df,
      group_params = base_params,
      interactive  = interactive,
      label_scheme = "letters"
    )
    df$baseGroup <- factor(as.character(df$baseGroup), ordered = TRUE)

    if (any(is.na(df$baseGroup))) {
      rlang::inform(
        "Some rows had NA baseGroup; assigning these to 'Unknown' and skipping subgroup findGroups() for them.",
        class = "srtm_analyse_traj_na_handled"
      )
      df$baseGroup <- forcats::fct_explicit_na(df$baseGroup, na_level = "Unknown")
    }

    rlang::inform(
      glue::glue(
        "SRTMAnalyse: now finding trajectory groups (trajGroup) within each baseGroup using `m01` and label_scheme = '{traj_label_scheme}'."
      ),
      class = "srtm_analyse_progress"
    )

    traj_params_by_base <- list()

    df <- df %>%
      dplyr::group_by(baseGroup) %>%
      dplyr::group_modify(function(.x, .g) {
        current_bg <- as.character(.g$baseGroup[[1]])
        n_rows     <- nrow(.x)
        n_nonmiss  <- sum(!is.na(.x$m01))
        if (identical(current_bg, "Unknown")) {
          .x$trajGroup <- factor("Unknown", levels = "Unknown", ordered = TRUE)
          return(.x)
        }
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
            "  Calling assignGroups() for trajGroups in baseGroup '{current_bg}' (label_scheme = '{traj_label_scheme}')."
          ),
          class = "srtm_analyse_progress"
        )

        .x$trajGroup <- assignGroups(
          data         = .x,
          group_params = traj_params,
          interactive  = interactive,
          label_scheme = "letters"
        )

        .x$trajGroup <- as.character(.x$trajGroup)
        .x
      }) %>%
      dplyr::ungroup()

    df$trajGroup <- factor(
      df$trajGroup,
      levels  = sort(unique(df$trajGroup)),
      ordered = TRUE
    )

    group_params_store$traj_by_base <- traj_params_by_base

  } else { # group_first == "slope"
    # 1) Trajectory groups on m01 (traj_label_scheme)
    # 2) Within each trajGroup, baseline groups on y1 (letters)

    rlang::inform(
      glue::glue(
        "SRTMAnalyse: finding trajectory groups (trajGroup) on `m01` for the whole sample (label_scheme = '{traj_label_scheme}')."
      ),
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
      "SRTMAnalyse: assigning trajGroup using assignGroups().",
      class = "srtm_analyse_progress"
    )

    df$trajGroup <- assignGroups(
      data         = df,
      group_params = traj_params_all,
      interactive  = interactive,
      label_scheme = "letters"
    )
    df$trajGroup <- factor(as.character(df$trajGroup), ordered = TRUE)

    if (any(is.na(df$trajGroup))) {
      rlang::inform(
        "Some rows had NA trajGroup; assigning these to 'Unknown' and skipping subgroup findGroups() for them.",
        class = "srtm_analyse_traj_na_handled"
      )
      df$trajGroup <- forcats::fct_explicit_na(df$trajGroup, na_level = "Unknown")
    }

    rlang::inform(
      glue::glue(
        "SRTMAnalyse: now finding baseline groups (baseGroup) within each trajGroup using `{y1_name}` (letters)."
      ),
      class = "srtm_analyse_progress"
    )

    base_params_by_traj <- list()

    df <- df %>%
      dplyr::group_by(trajGroup) %>%
      dplyr::group_modify(function(.x, .g) {
        current_tg <- as.character(.g$trajGroup[[1]])
        n_rows     <- nrow(.x)
        n_nonmiss  <- sum(!is.na(.x[[y1_name]]))
        if (identical(current_tg, "Unknown")) {
          .x$baseGroup <- factor("Unknown", levels = "Unknown", ordered = TRUE)
          return(.x)
        }

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
            "  Calling assignGroups() for baseGroups in trajGroup '{current_tg}' (letters)."
          ),
          class = "srtm_analyse_progress"
        )

        .x$baseGroup <- assignGroups(
          data         = .x,
          group_params = base_params,
          interactive  = interactive,
          label_scheme = "letters"
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

  # --- compute trajType using chosen trajectory label scheme ---------------
  df$trajType <- srtm_compute_trajType(
    data         = df,
    slope_col    = "m01",
    group_col    = "trajGroup",
    label_scheme = traj_label_scheme,
    dt01         = dt01,
    outcome_cols = c(y0_name, y1_name, y2_name)
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
    trajGroups = "trajType"
  )

  res2 <- compareSlopes(
    data       = df,
    obs        = "m12",
    exp        = "m01",
    baseGroups = "baseGroup",
    trajGroups = "trajType"
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
    traj_group = "trajType",
    drop       = TRUE
  )

  # --- assemble settings and pull slope thresholds -------------------------
  settings <- list(
    y0                = y0_name,
    y1                = y1_name,
    y2                = y2_name,
    id_col            = id_col,
    time01            = dt01,
    time12            = dt12,
    group_first       = group_first,
    traj_label_scheme = traj_label_scheme,
    time_mode         = .srtm_time_state$mode       %||% "duration",
    time_unit         = .srtm_time_state$unit_label %||% "Time units"
  )

  if (identical(.srtm_time_state$mode, "dates")) {
    settings$dates <- list(
      t0 = .srtm_time_state$t0,
      t1 = .srtm_time_state$t1,
      t2 = .srtm_time_state$t2
    )
  }

  thr_attr <- attr(df$trajType, "srtm_slope_thresholds", exact = TRUE)
  if (!is.null(thr_attr)) {
    settings$trajThresholds <- thr_attr
  }

  # --- assemble result object ----------------------------------------------
  out <- list(
    Comparisons        = res,
    Slope_Comparisons  = res2,
    GroupSummary       = group_summary,
    data               = df,
    settings           = settings,
    group_params       = group_params_store
  )

  class(out) <- c("srtm_analysis", class(out))
  out
}
