FitLLAIDSPos <- function(
  modelResults,
  info
) {
  N_HIV_Stage_S_Obs_5 <- NULL

  L_AIDSPos <- 0.0

  modelResults[, LL_AIDSPos_Year := 0]

  for (year in seq_len(nrow(modelResults))) {
    TotModel <- modelResults[year, N_HIV_Stage_S_Obs_5]
    TotData <- data[year, N_HIV_Stage_5]

    if (TotModel > 0 &&
        modelResults$Year[year] >= info$FitAIDSPosMinYear &&
        modelResults$Year[year] <= info$FitAIDSPosMaxYear)
    {
      if (info$ModelFitDist == 1) {
        modelResults[year, LL_AIDSPos_Year := FitLLPoisson(TotModel, TotData)]
      } else {
        stop('info$ModelFitDist != 1 not supported')
      }

      L_AIDSPos <- L_AIDSPos + modelResults[year, LL_AIDSPos_Year]
    }
  }

  return(L_AIDSPos)
}
