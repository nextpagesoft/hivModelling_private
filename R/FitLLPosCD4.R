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

  totModels <- modelResults[[N_HIV_Stage_S_Obs]]
  totDatas <- data[[N_HIV_Stage]]

  sel <-
    totModels > 0 &
    modelResults$Year >= info$FitPosCD4MinYear &
    modelResults$Year <= info$FitPosCD4MaxYear

  if (info$ModelFitDist == 'POISSON') {
    vals <- FitLLPoisson(totModels[sel], totDatas[sel])
  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    vals <- FitLLNegBin(totModels[sel], totDatas[sel], param$RDispRest)
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  modelResults[, (LL_PosCD4_Year) := 0]
  modelResults[sel, (LL_PosCD4_Year) := vals]

  ll <- sum(modelResults[[LL_PosCD4_Year]]) + sum(totModels < 0) * VERY_LRG

  return(ll)
}
