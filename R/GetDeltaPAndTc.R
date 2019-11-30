#' GetDeltaPAndTc
#'
#' Get \code{Delta} and \code{Tc} parameters based on intervals specification.
#'
#' @param intervals data.frame with specifications of intervals. Required.
#' @param maxYear Integer denoting last year for modelling. Required.
#' @param noStage Number of CD4 progression stages, including AIDS. Optional.
#'   Default = 5
#'
#' @return
#' List with DeltaP and Tc
#'
#' @examples
#' intervals <- data.table::data.table(
#'   StartYear = c(1980L, 1984L, 1996L, 2000L, 2005L, 2010L),
#'   Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
#'   DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#'   ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' )
#' GetDeltaPAndTc(intervals, 2016L)
#'
#' @export
GetDeltaPAndTc <- function(
  intervals,
  maxYear,
  noStage = 5
) {
  countIntervals <- nrow(intervals) + sum(intervals$Jump)

  deltaP <- matrix(0L, noStage, countIntervals)
  tc <- rep(0, countIntervals)

  rateIdx <- 0L
  tmpColIdx <- 0L

  for (i in seq_len(nrow(intervals))) {
    interval <- intervals[i]

    # Add extra column for jump
    if (interval$Jump) {
      if (!interval$DiffByCD4) {
        rateIdx <- rateIdx + 1
      }
      for (j in seq_len(noStage - 1)) {
        if (interval$DiffByCD4) {
          rateIdx <- rateIdx + 1
        }
        deltaP[j, tmpColIdx + 1] <- rateIdx
      }
      tmpColIdx <- tmpColIdx + 1
      tc[tmpColIdx] <- interval$StartYear
    }

    if (interval$ChangeInInterval && !interval$DiffByCD4) {
      rateIdx <- rateIdx + 1
    }
    for (j in seq_len(noStage - 1)) {
      if (interval$ChangeInInterval) {
        rateIdx <- rateIdx + ifelse(interval$DiffByCD4, 1, 0)
      }
      deltaP[j, tmpColIdx + 1] <- ifelse(interval$ChangeInInterval, rateIdx, 0)
    }
    tmpColIdx <- tmpColIdx + 1
    tc[tmpColIdx] <- interval$StartYear
  }

  tc[tmpColIdx + 1] <- maxYear + 1

  return(list(
    DeltaP = deltaP,
    Tc = tc
  ))
}
