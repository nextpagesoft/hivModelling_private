FitLLAIDS <- function(
  modelResults,
  info,
  data,
  param
) {
  # CRAN checks
  LL_AIDS_Year <- NULL

  totModels <- modelResults[['N_AIDS']]
  totDatas <- data[['N_AIDS']]

  sel <-
    totModels > 0 &
    modelResults$Year >= info$FitAIDSMinYear &
    modelResults$Year <= info$FitAIDSMaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  modelResults[, LL_AIDS_Year := 0]
  modelResults[sel, LL_AIDS_Year := vals]

  ll <- sum(modelResults$LL_AIDS_Year)

  return(ll)
}
