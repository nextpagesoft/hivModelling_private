FitLLAIDS <- function(
  modelResults,
  info,
  data
) {
  # CRAN checks
  LL_AIDS_Year <- NULL

  L_AIDS <- 0.0

  modelResults[, LL_AIDS_Year := 0]

  totModels <- modelResults[['N_AIDS']]
  totDatas <- data[['N_AIDS']]

  for (year in seq_len(nrow(modelResults))) {

    if (totModels[year] > 0 &&
        modelResults$Year[year] >= info$FitAIDSMinYear &&
        modelResults$Year[year] <= info$FitAIDSMaxYear)
    {
      if (info$ModelFitDist == 'POISSON') {
        set(x = modelResults,
            i = year,
            j = 'LL_AIDS_Year',
            value = FitLLPoisson(totModels[year], totDatas[year]))
      } else {
        stop('info$ModelFitDist different than "POISSON" is not yet supported')
      }

      L_AIDS <- L_AIDS + modelResults[['LL_AIDS_Year']][year]
    }
  }

  return(L_AIDS)
}
