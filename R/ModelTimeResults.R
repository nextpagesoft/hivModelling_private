ModelTimeResults <- function(
  years,
  info,
  param
) {
  ExportParametersToCpp(param, info)

  deltasList <- lapply(
    years,
    GetDelta,
    param$Delta4Fac, param$DeltaM, param$Tc, param$NoStage
  )

  timeToDiag <- sapply(deltasList, ModelTimeToDiag, param)
  timeToDiagMedian <- ModelTimeToDiagMedian(years)
  timeToDiagDist <- ModelTimeToDiagDist(info, param)

  return(list(
    Deltas = as.data.table(t(simplify2array(deltasList))),
    TimeToDiag = timeToDiag,
    TimeToDiagMedian = as.data.table(timeToDiagMedian),
    TimeToDiagDist = timeToDiagDist
  ))
}
