GetBSpline <- function(
  time,
  kOrder
) {
  bSpline <- matrix(0, info$ModelSplineN, kOrder)

  k <- 1
  for (i in seq_len(info$ModelSplineN)) {
    if (time >= info$MyKnots[i] && time < info$MyKnots[i + k]) {
      bSpline[i, k] <- 1
    }
  }

  for (k in seq(from = 2, to = kOrder)) {
    for (i in seq_len(info$ModelSplineN)) {
      if (time >= info$MyKnots[i] && time < info$MyKnots[i + k]) {
        if (info$MyKnots[i + k] != info$MyKnots[i + 1]) {
          bSpline[i, k] <- bSpline[i, k] +
            (info$MyKnots[i + k] - time) * bSpline[i + 1, k - 1] /
            (info$MyKnots[i + k] - info$MyKnots[i + 1])
        }

        if (info$MyKnots[i + k - 1] != info$MyKnots[i]) {
          bSpline[i, k] <- bSpline[i, k] +
            (time - info$MyKnots[i]) * bSpline[i, k - 1] /
            (info$MyKnots[i + k - 1] - info$MyKnots[i])
        }
      }
    }
  }

  val <- sum(param$Theta * bSpline[, kOrder])

  return(val)
}
