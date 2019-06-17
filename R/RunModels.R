#' Run all models
#'
#' Executes all the specified models
#'
#' @param argsList List of arguments to be passed to context (one per model
#'   run). Default = \code{NULL}
#' @param executionPlan Execution plan for the job planner (see package
#'   \pkg{future}). Default = \code{\link[future]{sequential}}
#'
#' @return
#' list object containing context, data and model objects
#'
#' @examples
#' RunModels()
#'
#' @export
RunModels <- function(
  argsList = NULL,
  executionPlan = future::sequential
) {
  # Set execution plan
  future::plan(executionPlan)

  # Create jobs
  jobs <- lapply(argsList, function(args) {
    future::future({
      do.call(RunIncidenceModel, args)
    })
  })

  # Collect results from jobs
  results <- future::values(jobs)

  return(results)
}
