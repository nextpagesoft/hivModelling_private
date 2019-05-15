FitLLPosCD4 <- function(
  modelResults,
  group,
  info
) {
  VERY_LRG <- 1.0e10
  L_PosCD4 <- 0.0

  N_HIV_Stage_S_Obs <- sprintf('N_HIV_Stage_S_Obs_%d', group)
  N_HIV_Stage <- sprintf('N_HIV_Stage_%d', group)
  LL_PosCD4_Year <- sprintf('LL_PosCD4_Year_%d', group)

  modelResults[, (LL_PosCD4_Year) := 0]

  # year <- 29
  for (year in seq_len(nrow(modelResults))) {
    TotModel <- modelResults[year, get(N_HIV_Stage_S_Obs)]
    TotData <- data[year, get(N_HIV_Stage)]

    if (
      TotModel > 0 &&
      modelResults$Year[year] >= info$FitPosCD4MinYear &&
      modelResults$Year[year] <= info$FitPosCD4MaxYear
    ) {
      if (info$ModelFitDist == 1) {
        modelResults[year, (LL_PosCD4_Year) := FitLLPoisson(TotModel, TotData)]
      } else {
        stop('info$ModelFitDist != 1 not supported')
      }
    }

    L_PosCD4 <- L_PosCD4 + modelResults[year, get(LL_PosCD4_Year)]
    if (TotModel < 0) {
      L_PosCD4 <- L_PosCD4 + VERY_LRG
    }
  }

  return(L_PosCD4)
}
