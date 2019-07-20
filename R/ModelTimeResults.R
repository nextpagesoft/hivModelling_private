ModelTimeResults <- function(
  modelResults,
  info,
  param
) {
  deltasList <- lapply(modelResults$Year, GetDelta, param)

  timeToDiag <- sapply(deltasList, ModelTimeToDiag, param)
  timeToDiagMedian <- lapply(modelResults$Year, ModelTimeToDiagMedian, param, info)
  timeToDiagDist <- ModelTimeToDiagDist(modelResults, info, param)

  return(list(
    TimeToDiag = timeToDiag,
    TimeToDiagMedian = timeToDiagMedian,
    TimeToDiagDist = timeToDiagDist
  ))
}
