GetBSpline <- function(
  time,
  param,
  info
) {
  kOrder <- info$ModelSplOrder
  myKnots <- info$MyKnots

  bSpline <- matrix(0, info$ModelSplineN, info$ModelSplOrder)

  bSpline[time >= myKnots[seq_len(info$ModelSplineN)] & time < myKnots[seq_len(info$ModelSplineN) + 1], 1] <- 1

  for (k in seq_len(kOrder - 1) + 1) {
    for (i in seq_len(info$ModelSplineN)) {
      if (time >= myKnots[i] && time < myKnots[i + k]) {
        if (myKnots[i + k] != myKnots[i + 1]) {
          bSpline[i, k] <-
            bSpline[i, k] +
            (myKnots[i + k] - time) * bSpline[i + 1, k - 1] /
            (myKnots[i + k] - myKnots[i + 1])
        }

        if (myKnots[i + k - 1] != myKnots[i]) {
          bSpline[i, k] <-
            bSpline[i, k] +
            (time - myKnots[i]) * bSpline[i, k - 1] /
            (myKnots[i + k - 1] - myKnots[i])
        }
      }
    }
  }

  val <- sum(param$Theta * bSpline[, kOrder])

  return(val)
}
