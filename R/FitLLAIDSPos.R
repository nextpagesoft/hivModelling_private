FitLLAIDSPos <- function(
  modelResults,
  info,
  data,
  param
) {
  LL_AIDSPos_Year <- NULL

  totModels <- modelResults[['N_HIV_Stage_S_Obs_5']]
  totDatas <- data[['N_HIV_Stage_5']]

  vals <- rep(0, length(totModels))
  sel <-
    totModels > 0 &
    modelResults$Year >= info$FitAIDSPosMinYear &
    modelResults$Year <= info$FitAIDSPosMaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals[sel] <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals[sel] <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  modelResults[, LL_AIDSPos_Year := vals]

  ll <- sum(vals)

  return(ll)
}
