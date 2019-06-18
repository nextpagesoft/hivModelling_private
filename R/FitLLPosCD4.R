FitLLPosCD4 <- function(
  modelResults,
  group,
  info,
  data,
  param
) {
  VERY_LRG <- 1.0e10
  L_PosCD4 <- 0.0

  N_HIV_Stage_S_Obs <- sprintf('N_HIV_Stage_S_Obs_%d', group)
  N_HIV_Stage <- sprintf('N_HIV_Stage_%d', group)
  LL_PosCD4_Year <- sprintf('LL_PosCD4_Year_%d', group)

  modelResults[, (LL_PosCD4_Year) := 0]

  totModels <- modelResults[[N_HIV_Stage_S_Obs]]
  totDatas <- data[[N_HIV_Stage]]

  for (year in seq_len(nrow(modelResults))) {

    if (
      totModels[year] > 0 &&
      modelResults$Year[year] >= info$FitPosCD4MinYear &&
      modelResults$Year[year] <= info$FitPosCD4MaxYear
    ) {
      if (info$ModelFitDist == 'POISSON') {
        set(x = modelResults,
            i = year,
            j = LL_PosCD4_Year,
            value = FitLLPoisson(totModels[year], totDatas[year]))
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        set(x = modelResults,
            i = year,
            j = LL_PosCD4_Year,
            value = FitLLNegBin(totModels[year], totDatas[year], param$RDispRest))
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }
    }

    L_PosCD4 <- L_PosCD4 + modelResults[[LL_PosCD4_Year]][year]
    if (totModels[year] < 0) {
      L_PosCD4 <- L_PosCD4 + VERY_LRG
    }
  }

  return(L_PosCD4)
}
