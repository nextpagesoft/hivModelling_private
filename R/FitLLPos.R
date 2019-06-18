FitLLPos <- function(
  modelResults,
  data,
  info,
  param
) {
  VERY_LRG <- 1.0e10

  lPos <- 0

  for (year in seq_len(nrow(modelResults))) {
    totModel <- modelResults$N_HIV_S_Obs[year]
    totData <- data$HIV$pop_0[year]

    if (totModel > 0 &&
        modelResults$Year[year] >= info$FitPosMinYear &&
        modelResults$Year[year] <= info$FitPosMaxYear &&
        (modelResults$Year[year] < info$FitPosCD4MinYear ||
         modelResults$Year[year] > info$FitPosCD4MaxYear)
    ) {
      if (info$ModelFitDist == 'POISSON') {
        llPosYear <- FitLLPoisson(totModel, totData)
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        llPosYear <- FitLLNegBin(totModel, totData, param$RDispRest)
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }

      lPos <- lPos + llPosYear
    }

    if (modelResults$N_Inf[year] < 0 || totModel < 0) {
      lPos <- lPos + VERY_LRG
    }
  }

  return(lPos)
}
