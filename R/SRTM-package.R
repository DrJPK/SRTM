#' SRTM: Self-Referenced Trajectory Modelling in Real Classrooms
#'
#' The **SRTM** (Self-Referenced Trajectory Modelling) package provides tools
#' for evaluating change in real classroom settings by treating **each
#' student as their own control**. Instead of relying on idealised randomised
#' control trials (RCTs), SRTM uses existing longitudinal data (historical,
#' baseline, post) to:
#'
#' \itemize{
#'   \item estimate each student's pre-influence trajectory,
#'   \item group students into baseline and trajectory profiles,
#'   \item predict what would likely have happened without the influence,
#'   \item compare expected vs observed post outcomes, and
#'   \item summarise these differences at group level.
#' }
#'
#' The main entry point is \code{\link{SRTMAnalyse}()}, which orchestrates the
#' full workflow from raw data to group-level comparisons and summaries.
#'
#' @section Teacher-proof demo workflow:
#'
#' This is a step-by-step workflow designed so that a teacher or practitioner
#' can run SRTM with minimal prior experience in R.
#'
#' \subsection{1. Installation}{
#'
#' Install the package from GitHub:
#'
#' \preformatted{
#' # install.packages("remotes")  # if needed
#' remotes::install_github("DrJPK/SRTM")
#' library(SRTM)
#' }
#' }
#'
#' \subsection{2. Load a demo dataset}{
#'
#' The package includes a synthetic demo dataset that looks like real
#' classroom data (three time points per student, plus IDs).
#'
#' \preformatted{
#' data("SRTM_synth_data")
#' x <- SRTM_synth_data
#'
#' str(x)
#' head(x)
#' }
#'
#' You should see columns similar to:
#'
#' \itemize{
#'   \item \code{y0} – historical measure (before the influence),
#'   \item \code{y1} – baseline / pre-measure (at onset of the influence),
#'   \item \code{y2} – post-measure (after the influence),
#'   \item an ID column (e.g. \code{ID} or \code{participantID}).
#' }
#' }
#'
#' \subsection{2B. Load your own Excel data}{
#'
#' If you have your own Excel file in the appropriate format, you can import
#' it using:
#'
#' \preformatted{
#' x <- importSRTMExcel("my_class_data.xlsx")
#' }
#'
#' The Excel file should have (at minimum):
#'
#' \itemize{
#'   \item one row per student,
#'   \item three numeric columns representing the three time points,
#'   \item an ID column (if missing, SRTM will create synthetic IDs).
#' }
#'
#' Check the structure with:
#'
#' \preformatted{
#' names(x)
#' }
#' }
#'
#' \subsection{3. Run the SRMT analysis}{
#'
#' Once your data frame \code{x} is ready, run:
#'
#' \preformatted{
#' res <- SRTMAnalyse(
#'   data        = x,
#'   y0          = y0,            # or your own column name
#'   y1          = y1,
#'   y2          = y2,
#'   id_col      = "participantID",  # or "ID", etc.
#'   group_first = "baseline",       # or "slope"
#'   interactive = TRUE
#' )
#' }
#'
#' What happens internally:
#'
#' \itemize{
#'   \item SRTM obtains the time intervals between the measurements
#'     (\code{time01} and \code{time12}) either from supplied values or via
#'     an interactive helper (\code{\link{getTimePeriod}()}).
#'   \item It calculates individual slopes between \code{y0} and \code{y1}
#'     (\code{m01}).
#'   \item It forms \emph{baseline groups} (e.g., low/medium/high) on
#'     \code{y1}, and \emph{trajectory groups} on \code{m01} (e.g. falling,
#'     stable, rising), either overall or nested (depending on
#'     \code{group_first}).
#'   \item It fits group-specific models to predict expected post scores
#'     (\code{exp_y2}) at \code{y2} given each student's prior trajectory.
#'   \item It compares observed vs expected outcomes using
#'     \code{\link{compareOutcomes}()} and summarises results with
#'     \code{\link{summariseGroupOutcomes}()}.
#' }
#' }
#'
#' \subsection{4. Visualise results}{
#'
#' After running \code{SRTMAnalyse()}, the result \code{res} is an object of
#' class \code{"srtm_analysis"}.
#'
#' If a plotting method is available, you can call:
#'
#' \preformatted{
#' plot(res)
#' }
#'
#' Typical plots may include:
#'
#' \itemize{
#'   \item trajectory plots showing how different groups change across time
#'     (y0, y1, y2),
#'   \item observed vs expected post scores for each baseline × trajectory
#'     group.
#' }
#'
#' Groups where observed > expected can be interpreted as doing \emph{better
#' than expected}; groups where observed < expected can be interpreted as
#' doing \emph{worse than expected}.
#' }
#'
#' \subsection{5. Inspect group-level summaries}{
#'
#' If a summary method is available, you can call:
#'
#' \preformatted{
#' summary(res)
#' }
#'
#' to see key group-level information, for each combination of baseline and
#' trajectory group:
#'
#' \itemize{
#'   \item group sizes,
#'   \item mean expected vs mean observed post scores,
#'   \item the difference (observed minus expected),
#'   \item p-values from the t-tests of observed vs expected.
#' }
#'
#' You can also access components directly:
#'
#' \preformatted{
#' res$Comparisons    # t-tests by baseline × trajectory group
#' res$GroupSummary   # descriptive summaries by group
#' }
#'
#' These show which kinds of students appear to benefit more, less, or not at
#' all from the influence under study.
#' }
#'
#' @section Conceptual rationale:
#'
#' Classical randomised control trials (RCTs) are difficult to implement in
#' authentic classroom contexts. Random assignment is often infeasible, and
#' there is no clear "standard treatment" in education comparable to
#' medicine. SRTM addresses this by treating \strong{each student as their own
#' control}, using their historical and baseline data to estimate where they
#' would likely have been at the post time point in the absence of a new
#' influence.
#'
#' SRTM is grounded in a set of behavioural principles:
#'
#' \subsection{0th Law: Behavioural Complexity}{
#'
#' Any observed behaviour results from complex, partly conscious and partly
#' subconscious processes involving attitudes, values and beliefs. The
#' "weights" linking these elements are:
#'
#' \itemize{
#'   \item numerous,
#'   \item dynamic and context-dependent,
#'   \item updated over time.
#' }
#'
#' Rather than attempting to measure these internal weights directly, SRTM
#' works at the level of observable measures and their trajectories over time.
#' }
#'
#' \subsection{1st Law: Behavioural Inertia}{
#'
#' In the absence of a net external influence, the rate at which a person's
#' attitudes, values and beliefs change is \emph{essentially} constant over
#' short periods in relatively stable contexts. This does not imply "no
#' change", but rather that the overall trend is approximately stable on the
#' scale at which we measure.
#'
#' SRTM uses this to say:
#'
#' \emph{If nothing new happens, a student's historical-to-baseline trajectory
#' can be extrapolated forward to estimate where they were heading.}
#' }
#'
#' \subsection{2nd Law: Behavioural Momentum}{
#'
#' When a net external influence is present (e.g., a new teaching strategy or
#' learning environment), the rate of change is approximately:
#'
#' \itemize{
#'   \item proportional to the \emph{perceived} strength of the influence,
#'   \item inversely proportional to the person's behavioural inertia.
#' }
#'
#' Students with very settled attitudes and values tend to change more slowly
#' than those with less established patterns. What matters is \emph{perceived}
#' influence: “what is heard” rather than merely what is said.
#'
#' SRTM interprets sustained differences between expected and observed change
#' as possible signals that an influence is strong enough to overcome
#' behavioural inertia for particular groups.
#' }
#'
#' \subsection{3rd Principle: Behavioural Impulse}{
#'
#' Influences rarely occur as single, isolated events. They either recur over
#' time or act continuously. The total effect of an influence depends on:
#'
#' \itemize{
#'   \item its perceived strength, and
#'   \item the length of time for which the person is exposed and susceptible.
#' }
#'
#' In SRTM, the choice of time gap between baseline and post must balance:
#'
#' \itemize{
#'   \item allowing enough time for genuine change to be detectable with the
#'     measurement tools available, and
#'   \item keeping the window short enough that the influence of interest is
#'     not swamped by many other uncontrolled influences.
#' }
#' }
#'
#' @section How SRTM uses these principles:
#'
#' In practice, SRTM proceeds as follows:
#'
#' \enumerate{
#'   \item Use historical (y0) and pre/baseline (y1) data to estimate each
#'     student's individual trajectory up to the onset of the influence.
#'   \item Assume, using behavioural inertia, that over a short period this
#'     trajectory would likely have continued in a similar way if nothing new
#'     had happened.
#'   \item Project this trajectory forward to estimate an expected post score
#'     at y2 for each student.
#'   \item Compare the expected score with the observed y2 for each student,
#'     and summarise these differences within baseline × trajectory groups.
#'   \item Use these group-level comparisons to help teachers and researchers
#'     decide which influences appear beneficial for which kinds of students.
#' }
#'
#' SRTM does \emph{not} attempt to identify a single "best practice" or to
#' dictate what teachers should do. Instead, it aims to:
#'
#' \itemize{
#'   \item simplify educational complexity just enough to make data
#'     interpretable,
#'   \item retain enough complexity to still reflect real classroom contexts,
#'   \item and support professional judgement by providing transparent,
#'     group-level evidence about who seems to benefit and who does not.
#' }
#'
#' @docType package
#' @name SRTM
#' @aliases SRTM-package
#' @keywords package
"_PACKAGE"
