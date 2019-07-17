FitLLPos <- function(
  modelResults,
  data,
  info,
  param
) {
  # CRAN checks
  LL_Pos_Year <- NULL
  VERY_LRG <- 1.0e10

  lPos <- 0

  modelResults[, LL_Pos_Year := 0]
  totModels <- modelResults[['N_HIV_S_Obs']]
  totDatas <- data[['N_HIV']]

  for (year in seq_len(nrow(modelResults))) {
    if (totModels[year] > 0 &&
        modelResults$Year[year] >= info$FitPosMinYear &&
        modelResults$Year[year] <= info$FitPosMaxYear &&
        (modelResults$Year[year] < info$FitPosCD4MinYear ||
         modelResults$Year[year] > info$FitPosCD4MaxYear)
    ) {
      if (info$ModelFitDist == 'POISSON') {
        set(x = modelResults,
            i = year,
            j = 'LL_Pos_Year',
            value = FitLLPoisson(totModels[year], totDatas[year]))
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        set(x = modelResults,
            i = year,
            j = 'LL_Pos_Year',
            value = FitLLNegBin(totModels[year], totDatas[year], param$RDispRest))
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }

      lPos <- lPos +  modelResults$LL_Pos_Year[year]
    }

    if (modelResults$N_Inf[year] < 0 || totModels[year] < 0) {
      lPos <- lPos + VERY_LRG
    }
  }

  return(lPos)
}
