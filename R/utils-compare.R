#' Internal helper: compute within-group differences and one-sample t-tests
#'
#' @description
#' `.srtm_compare_diffs()` is an internal utility used by functions such as
#' [compareOutcomes()] and [compareSlopes()].
#' It performs the “core” statistical work:
#'
#' \enumerate{
#'   \item For each \code{baseGroups × trajGroups} combination,
#'         extract the two numeric columns being compared
#'         (e.g., observed vs expected outcomes, or observed vs expected slopes).
#'   \item Compute the vector of differences \code{obs - exp}.
#'   \item Remove non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}).
#'   \item If fewer than two finite differences remain, return \code{NA}
#'         statistics for that group.
#'   \item If differences are constant (no variance), return the mean difference
#'         but skip the t-test (t and p are \code{NA}).
#'   \item Otherwise, run a one-sample t-test testing
#'         \eqn{H_0: E(obs - exp) = 0}.
#' }
#'
#' The function returns a tibble with one row per group and standard output
#' columns \code{t_value}, \code{diff}, \code{p}, \code{pretty_p}, and \code{df}.
#' Column naming is deliberately generic here; caller functions rename the
#' columns (e.g., \code{t_outcome}, \code{t_slope}) depending on context.
#'
#' @details
#' This function is not exported and is intended to eliminate duplicated logic
#' between the various comparison functions in the SRTM package.
#' Only minimal validation is performed—the caller is responsible for ensuring:
#' \itemize{
#'   \item \code{df} contains the grouping variables,
#'   \item \code{obs_name} and \code{exp_name} refer to numeric columns,
#'   \item grouping columns are named by \code{base_name} and \code{traj_name}.
#' }
#'
#' The returned columns:
#' \itemize{
#'   \item \strong{t_value}: t statistic from the one-sample test;
#'   \item \strong{diff}: mean of \code{obs - exp};
#'   \item \strong{p}: raw p-value;
#'   \item \strong{pretty_p}: formatted p-value (e.g., \code{"0.034"}, \code{"<.0001"});
#'   \item \strong{df}: degrees of freedom used in the test (\code{n - 1}).
#' }
#'
#' @param df A tibble or data frame containing at least four columns:
#'   \code{obs_name}, \code{exp_name}, and the grouping columns given by
#'   \code{base_name} and \code{traj_name}.
#'   All grouping and numeric validation must be performed upstream.
#'
#' @param obs_name A string giving the column name of the observed values.
#'
#' @param exp_name A string giving the column name of the expected values.
#'
#' @param base_name A string giving the column name used for baseline groups.
#'
#' @param traj_name A string giving the column name used for trajectory types.
#'
#' @return
#' A tibble with one row per \code{base_name × traj_name} group, containing
#' \code{t_value}, \code{diff}, \code{p}, \code{pretty_p}, and \code{df}.
#'
#' Column names remain generic here—the caller is expected to rename them to
#' context-specific names when joining them into larger summaries.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Example of how higher-level functions call .srtm_compare_diffs()
#'
#' # Suppose df contains:
#' #   y2       : observed outcome
#' #   exp_y2   : expected outcome
#' #   baseGroup
#' #   trajType
#'
#' # compareOutcomes() typically resolves names, ensures validity,
#' # then calls this helper like so:
#'
#' out_tbl <- .srtm_compare_diffs(
#'   df        = df,
#'   obs_name  = "y2",
#'   exp_name  = "exp_y2",
#'   base_name = "baseGroup",
#'   traj_name = "trajType"
#' )
#'
#' # The returned tibble contains:
#' #   baseGroup, trajType,
#' #   t_value, diff, p, pretty_p, df
#' # which compareOutcomes() will then rename to:
#' #   t_outcome, diff_outcome, p_outcome, ...
#' # before merging into the srtm_analysis object.
#' }

.srtm_compare_diffs <- function(df,
                                obs_name,
                                exp_name,
                                base_name,
                                traj_name) {

  df %>%
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
      } else if (pval < 1e-3) {
        "<.001"
      } else {
        formatC(pval, digits = 3, format = "fg")
      }

      # pretty_p <- if (is.na(pval)) {
      #   NA_character_
      # }else{
      #   cleaner::format_p_value(pval)
      # }

      tibble::tibble(
        t_value  = unname(tt$statistic),
        diff     = unname(tt$estimate),   # mean(obs - exp)
        p        = pval,
        pretty_p = pretty_p,
        df       = unname(tt$parameter)
      )
    }) %>%
    dplyr::ungroup() %>%
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
