FitLLAIDS <- function(
  modelResults,
  info,
  data,
  param
) {
  totModels <- modelResults[, 'N_AIDS']
  totDatas <- data[, 'N_AIDS']

  vals <- rep(0, length(totModels))
  sel <-
    totModels > 0 &
    modelResults[, 'Year'] >= info$FitAIDSMinYear &
    modelResults[, 'Year'] <= info$FitAIDSMaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals[sel] <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals[sel] <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispAIDS)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  ll <- sum(vals)

  return(list(
    LL = ll,
    Vals = vals,
    ColName = 'LL_AIDS_Year'
  ))
}
