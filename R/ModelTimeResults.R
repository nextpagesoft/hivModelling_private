ModelTimeResults <- function(
  modelResults,
  info,
  param
) {
  SetCountModelParameters(param, info)

  deltasList <- lapply(
    modelResults$Year,
    GetDelta,
    param$Delta4Fac, param$DeltaM, param$Tc, param$NoStage
  )
  timeToDiag <- sapply(deltasList, ModelTimeToDiag, param)
  timeToDiagMedian <- lapply(modelResults$Year, ModelTimeToDiagMedian)
  timeToDiagDist <- ModelTimeToDiagDist(modelResults, info, param)

  return(list(
    DeltasList = deltasList,
    TimeToDiag = timeToDiag,
    TimeToDiagMedian = timeToDiagMedian,
    TimeToDiagDist = timeToDiagDist
  ))
}
