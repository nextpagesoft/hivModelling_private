FitLLPosCD4 <- function(
  modelResults,
  group,
  info,
  data,
  param
) {
  VERY_LRG <- 1e+10

  N_HIV_Stage_S_Obs <- sprintf('N_HIV_Stage_S_Obs_%d', group)
  N_HIV_Stage <- sprintf('N_HIV_Stage_%d', group)
  LL_PosCD4_Year <- sprintf('LL_PosCD4_Year_%d', group)

  totModels <- modelResults[, N_HIV_Stage_S_Obs]
  totDatas <- data[, N_HIV_Stage]

  modelYears <- modelResults[, 'Year']

  vals <- rep(0, length(totModels))
  sel <-
    totModels > 0 &
    modelYears >= info$FitPosCD4MinYear &
    modelYears <= info$FitPosCD4MaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals[sel] <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals[sel] <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  ll <- sum(vals) + sum(totModels < 0) * VERY_LRG

  result <- cbind(vals)
  colnames(result) <- LL_PosCD4_Year

  return(list(
    LL = ll,
    Result = result
  ))
}

FitLLPosCD4DT <- function(
  modelResults,
  group,
  info,
  data,
  param
) {
  VERY_LRG <- 1e+10

  N_HIV_Stage_S_Obs <- sprintf('N_HIV_Stage_S_Obs_%d', group)
  N_HIV_Stage <- sprintf('N_HIV_Stage_%d', group)
  LL_PosCD4_Year <- sprintf('LL_PosCD4_Year_%d', group)

  totModels <- modelResults[[N_HIV_Stage_S_Obs]]
  totDatas <- data[[N_HIV_Stage]]

  vals <- rep(0, length(totModels))
  sel <-
    totModels > 0 &
    modelResults$Year >= info$FitPosCD4MinYear &
    modelResults$Year <= info$FitPosCD4MaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals[sel] <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals[sel] <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  modelResults[, (LL_PosCD4_Year) := vals]

  ll <- sum(vals) + sum(totModels < 0) * VERY_LRG

  return(ll)
}
