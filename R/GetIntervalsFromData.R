#' GetIntervalsFromData
#'
#' Get diagnosis rates intervals initialized from allowed year ranges
#'
#' @param minYear Start year for the diagnosis rate
#' @param maxYear End year for the diagnosis rate
#' @param numIntervals Number of intervals
#' @param firstIntervalEndYear End year of the first interval. Optional. Default = NULL
#'
#' @return
#' Intervals specification as data.table
#'
#' @examples
#' \dontrun{
#' GetIntervalsFromData(minYear, maxYear, numIntervals, firstIntervalEndYear)
#' }
#'
#' @export
GetIntervalsFromData <- function(
  minYear,
  maxYear,
  numIntervals,
  firstIntervalEndYear = NULL
) {
  if (is.null(minYear) || is.null(maxYear)) {
    return(NULL)
  }

  if (numIntervals < 1 || minYear > maxYear) {
    intervals <- data.table(
      StartYear = integer(),
      Jump = logical(),
      ChangeInInterval = logical(),
      DiffByCD4 = logical()
    )
    return(intervals)
  }

  startYear <- rep(NA, numIntervals)
  jump <- rep(NA, numIntervals)
  changeInInterval <- rep(NA, numIntervals)
  diffByCD4 <- rep(NA, numIntervals)

  i <- 1
  lastEndYear <- minYear
  for (i in seq_len(numIntervals)) {
    startYear[i] <- lastEndYear
    step <- max(floor((maxYear - startYear[i]) / (numIntervals - i + 1)), 1)
    endYear <- min(startYear[i] + step, maxYear)

    if (i == 1 && !is.null(firstIntervalEndYear) && firstIntervalEndYear >= minYear) {
      startYear[i] <- minYear
      endYear <- firstIntervalEndYear
    }

    jump[i] <- i != 1
    changeInInterval[i] <- FALSE
    diffByCD4[i] <- FALSE

    lastEndYear <- endYear
  }

  intervals <- data.table(
    StartYear = startYear,
    Jump = jump,
    ChangeInInterval = changeInInterval,
    DiffByCD4 = diffByCD4
  )

  intervals <- unique(intervals)

  return(intervals)
}
