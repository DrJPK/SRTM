#' Compare observed and expected outcomes within SRMT trajectory groups
#'
#' `compareOutcomes()` computes paired comparisons between an observed outcome
#' (e.g., post-transition scores at \code{y2}) and an expected outcome
#' (e.g., SRMT-predicted scores \code{exp_y2}) within combinations of baseline
#' and trajectory groups. For each \code{baseGroups} × \code{trajGroups}
#' combination, it performs a one-sample t-test on the difference
#' \code{obs - exp}, testing whether the mean difference is equal to zero.
#'
#' This function is typically used after SRMT models have been fitted to
#' generate expected trajectories, and after individuals have been assigned to
#' baseline and trajectory groups (e.g., via \code{\link{assignGroups}()} and
#' \code{\link{SRTMAnalyse}()}). It provides a compact summary of whether
#' particular groups are doing better or worse than expected at the observed
#' time-point.
#'
#' @param data A data frame or tibble containing the observed outcome,
#'   expected outcome, and grouping variables. Must include the columns
#'   referenced by \code{obs}, \code{exp}, \code{baseGroups}, and
#'   \code{trajGroups}.
#'
#' @param obs The observed outcome variable (default \code{y2}). This should
#'   be a numeric column in \code{data}. The argument is specified using tidy
#'   evaluation, so it is usually supplied as an unquoted column name
#'   (e.g., \code{obs = y2}).
#'
#' @param exp The expected outcome variable (default \code{exp_y2}). This
#'   should be a numeric column in \code{data}, typically generated from an
#'   SRMT model. Like \code{obs}, it is specified using an unquoted column
#'   name (e.g., \code{exp = exp_y2}).
#'
#' @param baseGroups A grouping variable representing baseline group membership
#'   (default \code{baseGroup}). This is usually an SRMT grouping derived from
#'   a baseline time-point (e.g., \code{y1}). It can be any column in
#'   \code{data} and is supplied as an unquoted column name.
#'
#' @param trajGroups A grouping variable representing trajectory group
#'   membership (default \code{trajGroup}). This is typically an SRMT
#'   trajectory classification that incorporates information from multiple
#'   time-points. Like \code{baseGroups}, it is supplied as an unquoted
#'   column name.
#'
#' @return A tibble with one row per \code{baseGroups} × \code{trajGroups}
#'   combination containing:
#'   \itemize{
#'     \item the grouping variables (with their original column names),
#'     \item \code{t_value}: the t statistic from the one-sample t-test on
#'       \code{obs - exp},
#'     \item \code{diff}: the mean difference \code{mean(obs - exp)},
#'     \item \code{p}: the raw p-value from the t-test,
#'     \item \code{pretty_p}: a formatted character version of the p-value
#'       (e.g., \code{"0.034"}, \code{"<.0001"}), or \code{NA} if not
#'       available,
#'     \item \code{df}: the degrees of freedom used in the t-test
#'       (typically \code{n - 1} for the number of non-missing differences).
#'   }
#'   Groups with fewer than two finite differences (\code{obs - exp}) receive
#'   \code{NA} for all test statistics. If all differences in a group are
#'   constant, \code{diff} is returned but the t-test is not computed
#'   (\code{t_value} and \code{p} are \code{NA}).
#'
#' @details
#' For each combination of \code{baseGroups} and \code{trajGroups},
#' \code{compareOutcomes()}:
#' \enumerate{
#'   \item Computes the vector of differences \code{obs - exp}.
#'   \item Removes non-finite values (e.g., \code{NA}, \code{NaN},
#'         \code{Inf}, \code{-Inf}).
#'   \item If fewer than two finite differences remain, returns \code{NA}
#'         statistics for that group.
#'   \item If the differences are constant (no variance), returns the mean
#'         difference but omits the t-test (t and p are \code{NA}).
#'   \item Otherwise, performs a one-sample t-test with null hypothesis
#'         \eqn{H_0: E(obs - exp) = 0} and reports the t statistic, p-value,
#'         mean difference, and degrees of freedom.
#' }
#'
#' The grouping variables are coerced to factors internally for cleaner
#' output, but their original names are preserved in the returned tibble.
#'
#' @seealso
#'   \code{\link{assignGroups}()} for creating groupings used as
#'   \code{baseGroups} and \code{trajGroups}, and \code{\link{SRTMAnalyse}()}
#'   for higher-level SRMT workflows that generate expected outcomes and group
#'   structures.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Suppose `dat` contains observed and expected y2 scores,
#' # plus baseline and trajectory groupings:
#' #   y2        : observed outcomes
#' #   exp_y2    : SRMT-expected outcomes
#' #   baseGroup : baseline grouping
#' #   trajGroup : trajectory grouping
#'
#' summary_tbl <- compareOutcomes(
#'   data        = dat,
#'   obs         = y2,
#'   exp         = exp_y2,
#'   baseGroups  = baseGroup,
#'   trajGroups  = trajGroup
#' )
#'
#' # Inspect groups doing better or worse than expected:
#' summary_tbl %>%
#'   arrange(p)
#' }
#'
#' @section Interpretation:
#'
#' The key quantity produced by `compareOutcomes()` is the mean difference
#' \code{diff = mean(obs - exp)} within each \code{baseGroups} ×
#' \code{trajGroups} combination.
#'
#' \subsection{Signs and magnitudes of differences}{
#' \itemize{
#'   \item A \strong{positive} value of \code{diff} indicates that, on average,
#'     students in the group achieved \emph{higher} observed outcomes than
#'     expected based on their SRMT trajectory. This can be interpreted as
#'     better-than-expected performance.
#'
#'   \item A \strong{negative} value of \code{diff} indicates that students'
#'     observed outcomes were \emph{lower} than SRMT expectations. This can be
#'     interpreted as lower-than-expected performance.
#'
#'   \item A value close to zero suggests that the group performed in line with
#'     expectations.
#' }
#' }
#'
#' \subsection{Understanding the t-test}{
#' For each group, a one-sample t-test evaluates whether the mean difference
#' \code{obs - exp} is statistically distinguishable from zero. A significant
#' p-value indicates evidence that the group is performing either better or
#' worse than expected.
#'
#' The function returns both the raw p-value (\code{p}) and a formatted
#' version (\code{pretty_p}) that matches typical reporting conventions.
#' }
#'
#' \subsection{Role of baseline vs trajectory groups}{
#' \code{baseGroups} represent students' initial standing (e.g., based on
#' a baseline time-point such as \code{y1}).
#'
#' \code{trajGroups} reflect students’ SRMT trajectory classification across
#' time.
#'
#' Comparing outcomes across both dimensions helps identify:
#' \itemize{
#'   \item whether particular baseline groups are improving or declining,
#'   \item whether specific trajectory groups are aligned with expectations,
#'   \item interactions between where students started and how they developed
#'         through the transition.
#' }
#' }
#'
#' \subsection{Practical use}{
#' In SRMT workflows, these comparisons help schools or analysts understand
#' which groups of students — defined by their starting point and developmental
#' trajectory — are:
#' \itemize{
#'   \item exceeding expectations,
#'   \item meeting expectations, or
#'   \item falling short of expectations.
#' }
#'
#' This facilitates targeted interpretation and supports better-informed
#' follow-up actions when examining transition data or intervention effects.
#' }
#'
#' @export

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
