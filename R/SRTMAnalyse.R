#' Run a full Self-Referenced Trajectory Modelling (SRMT) analysis
#'
#' `SRTMAnalyse()` is the main entry point for the SRTM package. Given repeated
#' measures for three time points (y0, y1, y2), it:
#' \itemize{
#'   \item sets up the time scale between measurements (either via supplied
#'     durations or interactively via dates or durations),
#'   \item computes individual slopes between y0 and y1,
#'   \item forms baseline groups and trajectory groups using
#'     \code{\link{findGroups}()} and \code{\link{assignGroups}()},
#'   \item fits group-specific linear models to predict post scores at y2,
#'   \item compares observed and expected outcomes using
#'     \code{\link{compareOutcomes}()},
#'   \item and produces group-level summaries via
#'     \code{\link{summariseGroupOutcomes}()}.
#' }
#'
#' The result is a structured object of class \code{"srtm_analysis"} that
#' contains:
#' \itemize{
#'   \item the augmented analysis data (with IDs, time variables, slopes,
#'     groups, expected outcomes, etc.),
#'   \item group-level comparisons of observed vs expected outcomes,
#'   \item compact summaries for each baseline × trajectory group, and
#'   \item settings and grouping parameters used to construct the model.
#' }
#'
#' @param data A data frame or tibble containing at least three numeric
#'   columns representing repeated measurements at three time points. These
#'   columns are identified by \code{y0}, \code{y1}, and \code{y2}. The data
#'   may also contain an ID column used to identify individuals.
#'
#' @param y0,y1,y2 Variables representing the three time points used in the
#'   SRMT workflow. By default these are the character strings \code{"y0"},
#'   \code{"y1"}, and \code{"y2"}, but they can also be supplied as unquoted
#'   column names (e.g., \code{y0 = baseline_score}). Internally these
#'   arguments are converted to column names and used to:
#'   \itemize{
#'     \item calculate slopes between \code{y0} and \code{y1},
#'     \item define baseline groupings on \code{y1},
#'     \item and fit linear models to predict \code{y2}.
#'   }
#'
#' @param id_col Character string giving the name of the ID column in
#'   \code{data}. If the specified column exists, it is coerced to a factor
#'   and used to identify individuals. If it does not exist, synthetic IDs of
#'   the form \code{"ID000001"}, \code{"ID000002"}, … are generated and added
#'   to the data, and an informational message is emitted.
#'
#' @param time01,time12 Optional numeric values giving the time differences
#'   between the three measurement occasions:
#'   \itemize{
#'     \item \code{time01}: the time interval between \code{y0} and \code{y1},
#'     \item \code{time12}: the time interval between \code{y1} and \code{y2}.
#'   }
#'   If supplied, they must be single positive numeric values and are used
#'   directly. If either is \code{NULL} and \code{interactive = TRUE}, the
#'   function delegates to \code{\link{getTimePeriod}()} to obtain the
#'   required time intervals interactively (either via durations or dates). If
#'   either is \code{NULL} and \code{interactive = FALSE}, the function aborts
#'   with an error.
#'
#' @param group_first Character string indicating the order in which groups
#'   should be formed. Must be one of:
#'   \itemize{
#'     \item \code{"baseline"}: first form baseline groups on \code{y1} for
#'       the whole sample, then form trajectory groups on the slope between
#'       \code{y0} and \code{y1} (\code{m01}) within each baseline group.
#'     \item \code{"slope"}: first form trajectory groups on \code{m01} for
#'       the whole sample, then form baseline groups on \code{y1} within each
#'       trajectory group.
#'   }
#'   This allows users to emphasise either initial status (\emph{baseline
#'   first}) or early change (\emph{slope first}) in the grouping structure.
#'
#' @param interactive Logical. If \code{TRUE} (default), the function is
#'   allowed to use interactive helpers:
#'   \itemize{
#'     \item \code{\link{getTimePeriod}()} may prompt for time intervals and
#'       time-unit labels or dates,
#'     \item \code{\link{findGroups}()} may display plots and prompt for
#'       grouping decisions.
#'   }
#'   If \code{FALSE}, any required time intervals must be supplied via
#'   \code{time01} and \code{time12}, and any grouping decisions must be made
#'   non-interactively (according to the behaviour of \code{findGroups()} and
#'   \code{assignGroups()} when \code{interactive = FALSE}).
#'
#' @return An object of class \code{"srtm_analysis"}, which is a named list
#'   with the following components:
#'   \describe{
#'     \item{Comparisons}{A tibble returned by \code{\link{compareOutcomes}()},
#'       containing one row per baseline × trajectory group with t-tests of
#'       observed vs expected outcomes at \code{y2}.}
#'
#'     \item{GroupSummary}{A tibble returned by
#'       \code{\link{summariseGroupOutcomes}()}, providing descriptive
#'       statistics (e.g., group sizes, means, expected values) for each
#'       baseline × trajectory group.}
#'
#'     \item{data}{The original data augmented with additional columns used in
#'       the SRMT analysis, including (but not limited to):
#'       \itemize{
#'         \item the ID column specified by \code{id_col} (or synthetic IDs),
#'         \item \code{dt01} and \code{dt12}: time intervals between
#'           \code{y0}–\code{y1} and \code{y1}–\code{y2},
#'         \item \code{t0}, \code{t1}, \code{t2}: time positions for each
#'           occasion (either numeric, with \code{t1 = 0}, or Dates in "dates"
#'           mode),
#'         \item \code{m01}: the slope between \code{y0} and \code{y1},
#'         \item \code{baseGroup}: baseline group membership,
#'         \item \code{trajGroup}: trajectory group membership,
#'         \item \code{trajType}: a labelled trajectory type derived from
#'           \code{m01} and \code{trajGroup},
#'         \item \code{exp_y2}: the expected post score from group-specific
#'           linear models.
#'       }}
#'
#'     \item{settings}{A list of analysis settings used to construct the
#'       model, including:
#'       \itemize{
#'         \item \code{y0}, \code{y1}, \code{y2}: the names of the time-point
#'           columns,
#'         \item \code{id_col}: the name of the ID column,
#'         \item \code{time01}, \code{time12}: the numeric time intervals used,
#'         \item \code{group_first}: the grouping order (\code{"baseline"} or
#'           \code{"slope"}),
#'         \item \code{time_mode}: \code{"duration"} or \code{"dates"}
#'           depending on how timing was specified,
#'         \item \code{time_unit}: a label for the time units (e.g. "days",
#'           "weeks", "school terms"),
#'         \item \code{dates}: in dates mode, a list with \code{t0}, \code{t1},
#'           \code{t2} storing the original Historical, Pre, and Post dates,
#'         \item \code{trajThresholds}: optional slope thresholds used to
#'           label trajectories (if provided by
#'           \code{\link{srtm_compute_trajType}()}).
#'       }}
#'
#'     \item{group_params}{A list of \code{"srtm_group_suggestion"} objects
#'       returned by \code{\link{findGroups}()}, used to construct the
#'       baseline and trajectory groupings. The list typically contains:
#'       \itemize{
#'         \item \code{baseline_overall}: grouping of \code{y1} for the whole
#'           sample (when \code{group_first = "baseline"}),
#'         \item \code{traj_overall}: grouping of \code{m01} for the whole
#'           sample (when \code{group_first = "slope"}),
#'         \item \code{traj_by_base}: grouping objects for \code{m01} within
#'           each baseline group (when \code{group_first = "baseline"}),
#'         \item \code{baseline_by_traj}: grouping objects for \code{y1}
#'           within each trajectory group (when
#'           \code{group_first = "slope"}).
#'       }}
#'   }
#'
#'   The object can be inspected directly, or passed to plot/summary methods
#'   (if provided) to generate visualisations and reports.
#'
#' @section Workflow:
#'
#' Internally, `SRTMAnalyse()` proceeds through the following stages:
#' \enumerate{
#'   \item \strong{Time setup}:
#'     \itemize{
#'       \item Resets the internal time state via
#'         \code{\link{srtm_reset_time_state}()}.
#'       \item Obtains \code{dt01} and \code{dt12} either from
#'         \code{time01}/\code{time12} or interactively via
#'         \code{\link{getTimePeriod}()}.
#'       \item Constructs \code{t0}, \code{t1}, \code{t2} as either numeric
#'         times (with \code{t1 = 0}, \code{t0 = -dt01}, \code{t2 = dt12}) or,
#'         in dates mode, as the original Historical/Pre/Post dates.
#'     }
#'
#'   \item \strong{Slope calculation}:
#'     \itemize{
#'       \item Computes \code{m01} (the slope between \code{y0} and
#'         \code{y1}) using \code{\link{calculateTrajectories}()}.
#'     }
#'
#'   \item \strong{Grouping}:
#'     \itemize{
#'       \item If \code{group_first = "baseline"}:
#'         \enumerate{
#'           \item Use \code{\link{findGroups}()} on \code{y1} for the whole
#'             sample to define baseline groups.
#'           \item Use \code{\link{assignGroups}()} to assign \code{baseGroup}
#'             to each individual.
#'           \item Within each \code{baseGroup}, use \code{findGroups} on
#'             \code{m01} and \code{assignGroups} to create trajectory groups
#'             (\code{trajGroup}), typically labelled using an arrow-based
#'             scheme.
#'         }
#'       \item If \code{group_first = "slope"}:
#'         \enumerate{
#'           \item Use \code{findGroups} on \code{m01} for the whole sample to
#'             define trajectory groups.
#'           \item Use \code{assignGroups} to assign \code{trajGroup} to each
#'             individual.
#'           \item Within each \code{trajGroup}, use \code{findGroups} on
#'             \code{y1} and \code{assignGroups} to create baseline groups
#'             (\code{baseGroup}).
#'         }
#'     }
#'
#'   \item \strong{Trajectory labelling}:
#'     \itemize{
#'       \item Calls \code{\link{srtm_compute_trajType}()} to construct
#'         \code{trajType}, a human-readable label for trajectory patterns
#'         based on \code{m01}, \code{trajGroup}, and the time interval
#'         \code{dt01}.
#'     }
#'
#'   \item \strong{Outcome modelling and comparison}:
#'     \itemize{
#'       \item Fits group-specific linear models and computes expected post
#'         values \code{exp_y2} via \code{\link{predictPostResponse}()}.
#'       \item Runs \code{\link{compareOutcomes}()} to compare observed
#'         \code{y2} with \code{exp_y2} within each baseline × trajectory
#'         group using paired t-tests.
#'       \item Summarises group outcomes with
#'         \code{\link{summariseGroupOutcomes}()}.
#'     }
#' }
#'
#' @seealso
#'   \code{\link{calculateTrajectories}()},
#'   \code{\link{findGroups}()},
#'   \code{\link{assignGroups}()},
#'   \code{\link{getTimePeriod}()},
#'   \code{\link{compareOutcomes}()},
#'   \code{\link{summariseGroupOutcomes}()},
#'   \code{\link{srtm_compute_trajType}()},
#'   \code{\link{predictPostResponse}()}.
#'
#' @examples
#' \dontrun{
#' # Basic usage with a three-time-point dataset
#' library(dplyr)
#'
#' # Assume `SRTM_student_attitude_data` has columns y0, y1, y2 and participantID
#' data("SRTM_student_attitude_data")
#'
#' fit <- SRTMAnalyse(
#'   data        = SRTM_student_attitude_data,
#'   y0          = y0,
#'   y1          = y1,
#'   y2          = y2,
#'   id_col      = "participantID",
#'   group_first = "baseline",
#'   interactive = TRUE
#' )
#'
#' # Group-level comparisons of observed vs expected post scores
#' fit$Comparisons
#'
#' # Group-level descriptive summaries
#' fit$GroupSummary
#'
#' # Augmented analysis data with groups and expected outcomes
#' dplyr::glimpse(fit$data)
#'
#' # Access analysis settings
#' fit$settings
#' }
#'
#' @export
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
