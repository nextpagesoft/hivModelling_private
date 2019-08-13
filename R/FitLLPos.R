FitLLPos <- function(
  modelResults,
  data,
  info,
  param
) {
  # CRAN checks
  LL_Pos_Year <- NULL
  VERY_LRG <- 1e+10

  totModels <- modelResults[['N_HIV_S_Obs']]
  totDatas <- data[['N_HIV']]

  sel <-
    totModels > 0 &
    modelResults$Year >= info$FitPosMinYear &
    modelResults$Year <= info$FitPosMaxYear &
    (modelResults$Year < info$FitPosCD4MinYear | modelResults$Year > info$FitPosCD4MaxYear)

  if (info$ModelFitDist == 'POISSON') {
    vals <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  modelResults[, LL_Pos_Year := 0]
  modelResults[sel, LL_Pos_Year := vals]

  ll <- sum(modelResults$LL_Pos_Year) + sum(modelResults$N_Inf < 0 | totModels < 0) * VERY_LRG

  return(ll)
}
