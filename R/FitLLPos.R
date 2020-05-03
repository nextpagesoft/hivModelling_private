FitLLPos <- function(
  modelResults,
  data,
  info,
  param
) {
  VERY_LRG <- 1e+10

  totModels <- modelResults[, 'N_HIV_S_Obs']
  totDatas <- data[, 'N_HIV']
  modelYears <- modelResults[, 'Year']

  vals <- rep(0, length(totModels))
  sel <-
    totModels > 0 &
    modelYears >= info$FitPosMinYear &
    modelYears <= info$FitPosMaxYear &
    (modelYears < info$FitPosCD4MinYear | modelYears > info$FitPosCD4MaxYear)

  if (info$ModelFitDist == 'POISSON') {
    vals[sel] <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals[sel] <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  ll <- sum(vals) + sum(modelResults[, 'N_Inf'] < 0 | totModels < 0) * VERY_LRG

  return(list(
    LL = ll,
    Vals = vals,
    ColName = 'LL_Pos_Year'
  ))
}
