#' PerformBootstrapFits
#'
#' Performs multiple bootstrap fits
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param mainResults Main results. Required.
#' @param bsCount Count of boostrap iterations. Default = \code{20}
#' @param maxNoFit Maximum number of optimization iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in optimization calculations. Optional.
#'   Default = 1e-5.
#' @param executionPlan Execution plan for the job planner (see package 'future'). Optional.
#'   Default = future::sequential.
#' @param statusRefreshRate Number of seconds to next refresh of job status. Optional. Default = 2.
#' @param verbose Logical indicating to print detailed info during fitting. Optional.
#'   Default = \code{FALSE}
#'
#' @return
#' list object containing context, data and model objects
#'
#' @examples
#' \dontrun{
#' PerformBootstrapFits(bsCount, context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose)
#' }
#'
#' @export
PerformBootstrapFits <- function(
  context,
  data,
  mainResults,
  bsCount = 20,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  executionPlan = future::sequential,
  statusRefreshRate = 2,
  verbose = FALSE
) {
  # Set execution plan
  future::plan(executionPlan)

  # Create jobs
  jobs <- lapply(seq_len(bsCount), function(idx) {
    message(sprintf('Performing iteration %d', idx))
    future::future({
      PerformBootstrapFit(idx, context, data, mainResults, maxNoFit, ctol, ftol, verbose)
    })
  })

  # Monitor jobs and check results.
  iterResults <- MonitorFutureJobs(jobs, statusRefreshRate)

  # Add confidence bounds
  confBounds <- ComputeConfidenceBounds(iterResults)

  # Create results object
  results <- modifyList(
    list(IterResults = iterResults),
    list(ConfBounds = confBounds)
  )

  return(results)
}
