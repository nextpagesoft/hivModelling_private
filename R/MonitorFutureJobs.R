#' MonitorFutureJobs
#'
#' Monitors the progress of jobs defined with package 'future'.
#'
#' @param jobs List of 'future' jobs.
#' @param statusRefreshRate Number of seconds to next refresh of job status. Default: 2
#'
#' @return
#' The results of running the futures. This will evaluate any results so if the result is an error
#' (for example triggered by stop) the error will be triggered and execution will stop.
#'
#' @examples
#' \dontrun{
#' MonitorFutureJobs(jobs)
#' }
#'
#' @export
MonitorFutureJobs <- function(
  jobs,
  statusRefreshRate = 2
) {
  pb <- txtProgressBar(0, length(jobs), style = 3)
  on.exit({
    close(pb)
  })

  repeat {
    jobStatuses <- sapply(jobs, future::resolved)
    setTxtProgressBar(pb, sum(jobStatuses))
    if (all(jobStatuses)) {
      cat('\n')
      break
    }
    Sys.sleep(statusRefreshRate)
  }

  # 'try' block needed to prevent stopping at first error, thus having
  # results not populated for all jobs
  results <- lapply(jobs, function(job) {
    try(future::value(job), silent = TRUE)
  })

  # Jobs may have no names, thus results will be nameless. We add generic names for printing.
  if (is.null(names(results))) {
    names(results) <- seq_along(jobs)
  }

  # Check for errors in the results ----------------------------------------------------------------
  errs <- sapply(results, inherits, 'try-error')
  if (any(errs)) {
    errorJobNames <- names(errs)[errs]

    # Remove 'Error :' to clean up display.
    # Collapse without line breaks, error messages already include them.
    details <- sapply(errorJobNames, function(jobName) {
      sprintf('Job \'%s\': %s', jobName, sub('^Error: ', '', results[[jobName]]))
    })
    details <- paste(details, collapse = '')

    # No line breaks in sprintf as they become part of formatting
    stop(
      sprintf(
        'The following jobs thrown errors during execution:\n%s',
        details
      )
    )
  }

  return(results)
}
