FitLLTotal <- function(
  p,
  probSurv1996,
  param,
  info,
  data,
  detailedResults = TRUE
) {
  VERY_LRG <- 1e+10
  # Smoothness omitted
  smooth1 <- 0
  smooth2 <- 0

  param$DeltaM <- GetParamDeltaM(p, param)
  param$Theta <- GetParamTheta(p, param, info)

  SetCountModelParameters(param, info)
  res <- CountOdeintLoop()

  modelResults <- res[['ModelResults']]
  minLambda <- res[['MinLambda']]

  # Changes made by reference
  modelResults <- cbind(
    modelResults,
    ModelAnnualNumbers(modelResults, probSurv1996, data[, 'Prob_CD4'])
  )

  if (minLambda < 0) {
    lambdaPenalty <- VERY_LRG
  } else {
    lambdaPenalty <- 0
  }

  llPosCD4_1 <- FitLLPosCD4(modelResults, group = 1, info, data, param)
  llPosCD4_2 <- FitLLPosCD4(modelResults, group = 2, info, data, param)
  llPosCD4_3 <- FitLLPosCD4(modelResults, group = 3, info, data, param)
  llPosCD4_4 <- FitLLPosCD4(modelResults, group = 4, info, data, param)
  llPos <- FitLLPos(modelResults, data, info, param)
  llAIDSPos <- FitLLAIDSPos(modelResults, info, data, param)
  llAIDS <- FitLLAIDS(modelResults, info, data, param)

  llTotal <-
    lambdaPenalty +
    llPosCD4_1$LL +
    llPosCD4_2$LL +
    llPosCD4_3$LL +
    llPosCD4_4$LL +
    llPos$LL +
    llAIDSPos$LL +
    llAIDS$LL +
    param$Smoothing1 * smooth1 +
    param$Smoothing2 * smooth2 +
    sum(p[1:3] > 2) * VERY_LRG

  if (detailedResults) {
    llResults <- cbind(
      llPosCD4_1$Vals, llPosCD4_2$Vals, llPosCD4_3$Vals, llPosCD4_4$Vals, llPos$Vals,
      llAIDSPos$Vals, llAIDS$Vals
    )
    colnames(llResults) <- c(
      llPosCD4_1$ColName, llPosCD4_2$ColName, llPosCD4_3$ColName, llPosCD4_4$ColName, llPos$ColName,
      llAIDSPos$ColName, llAIDS$ColName
    )

    modelResults <- cbind(modelResults, llResults)

    result <- list(
      LLTotal = llTotal,
      ModelResults = modelResults
    )
  } else {
    result <- llTotal
  }

  return(result)
}
