FitLLAIDSPos <- function(
  modelResults,
  info,
  data,
  param
) {
  LL_AIDSPos_Year <- NULL

  L_AIDSPos <- 0.0

  modelResults[, LL_AIDSPos_Year := 0]

  totModels <- modelResults[['N_HIV_Stage_S_Obs_5']]
  totDatas <- data[['N_HIV_Stage_5']]

  for (year in seq_len(nrow(modelResults))) {

    if (totModels[year] > 0 &&
        modelResults$Year[year] >= info$FitAIDSPosMinYear &&
        modelResults$Year[year] <= info$FitAIDSPosMaxYear)
    {
      if (info$ModelFitDist == 'POISSON') {
        set(x = modelResults,
            i = year,
            j = 'LL_AIDSPos_Year',
            value = FitLLPoisson(totModels[year], totDatas[year]))
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        set(x = modelResults,
            i = year,
            j = 'LL_AIDSPos_Year',
            value = FitLLNegBin(totModels[year], totDatas[year], param$RDispRest))
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }

      L_AIDSPos <- L_AIDSPos + modelResults[['LL_AIDSPos_Year']][year]
    }
  }

  return(L_AIDSPos)
}
