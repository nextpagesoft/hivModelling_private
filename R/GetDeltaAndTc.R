#' GetDeltaAndTc
#'
#' Returns delta and tc parameters
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' GetDeltaAndTc()
#'
#' @export
GetDeltaAndTc <- function()
{

  intervals <- data.table(
    StartYear = c(1980L, 1984L, 1996L, 2000L, 2005L, 2010L),
    EndYear = c(1984L, 1996L, 2000L, 2005L, 2010L, 2016L),
    Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  delta <- matrix(0, 4, nrow(intervals) + sum(intervals$Jump))
  tc <- rep(0, nrow(intervals) + sum(intervals$Jump))

  countCD4Cat <- 4
  rateIdx <- 0
  tmpColIdx <- 0

  for (i in seq_len(nrow(intervals))) {
    interval <- intervals[i]

    # Add extra column for jump
    if (interval$Jump) {

      if (!interval$DiffByCD4) {
        rateIdx <- rateIdx + 1
      }
      for (j in seq_len(countCD4Cat)) {
        if (interval$DiffByCD4) {
          rateIdx <- rateIdx + 1
        }
        delta[j, tmpColIdx] <- rateIdx
      }
      tmpColIdx <- tmpColIdx + 1
      tc[tmpColIdx] <- interval$StartYear
    }

    # Second column
    if (interval$ChangeInInterval && !interval$DiffByCD4) {
      rateIdx <- rateIdx + 1
    }
    for (j in seq_len(countCD4Cat)) {
      if (interval$ChangeInInterval) {
        rateIdx <- rateIdx + ifelse(interval$DiffByCD4, 1, 0)
      }
      delta[j, tmpColIdx] <- ifelse(interval$ChangeInInterval, rateIdx, 0)
    }
    tmpColIdx <- tmpColIdx + 1
    tc[tmpColIdx] <- interval$StartYear
  }
}
