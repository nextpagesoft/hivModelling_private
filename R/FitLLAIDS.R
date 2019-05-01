FitLLAIDS <- function(
  modelResults,
  info,
  extraResults
) {
  L_AIDS <- 0.0

  for (year in seq_len(nrow(modelResults))) {
    TotModel <- modelResults[year, N_AIDS]
    TotData <- data[year, N_AIDS]

    if (TotModel > 0 &&
        modelResults$Year[year] >= info$FitAIDSMinYear &&
        modelResults$Year[year] <= info$FitAIDSMaxYear)
    {
      if (info$ModelFitDist == 1) {
        extraResults[year, LL_AIDS_Year := FitLLPoisson(TotModel, TotData)]
      } else {
        stop('info$ModelFitDist != 1 not supported')
      }

      L_AIDS <- L_AIDS + extraResults[year, LL_AIDS_Year]
    }
  }

  return(L_AIDS)
}
