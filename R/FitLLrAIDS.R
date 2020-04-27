FitLLrAIDS <- function(
  r,
  extraArgs
) {
  modelResults <- extraArgs$ModelResults
  data <- extraArgs$Data
  info <- extraArgs$Info

  psiry <- 0

  totModels <- modelResults[, 'N_AIDS']
  totDatas <- data[, 'N_AIDS']

  for (year in seq_len(nrow(modelResults))) {
    if (
      totModels[year] > 0 &&
      modelResults[, 'Year'] >= info$FitAIDSMinYear &&
      modelResults[, 'Year'] <= info$FitAIDSMaxYear
    ) {
      psiry <- psiry +
        digamma(r + totDatas[year]) -
        digamma(r) +
        (totModels[year] - totDatas[year]) / (r + totModels[year]) +
        log(r / (r + totModels[year]))
    }
  }

  return(psiry)
}
