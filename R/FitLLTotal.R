FitLLTotal <- function(
  p,
  probSurv1996,
  param,
  info,
  data
) {
  # CRAN checks
  Year <- NULL
  Prob_CD4 <- NULL

  VERY_LRG <- 1e+10
  # Smoothness omitted
  smooth1 <- 0
  smooth2 <- 0

  # Force numeric type to avoid conversion from integer to numeric later
  modelYears <- as.numeric(info$ModelMinYear:info$ModelMaxYear)

  param$DeltaM <- GetParamDeltaM(p, param)
  param$Theta <- GetParamTheta(p, param, info)

  derivsFunc <- GetDerivsFuncXptr('CountModel')
  res <- odeintLoop(modelYears, param, info, derivsFunc)

  modelResults <- res[['ModelResults']]
  minLambda <- res[['MinLambda']]

  modelResults <- cbind(
    modelYears[-length(modelYears)],
    modelResults
  )
  colnames(modelResults) <- c(
    'Year',
    'PrimInf',
    paste0('Undiagnosed_', seq_len(param$NoStage)),
    paste0('Diagnosed_', seq_len(param$NoStage)),
    paste0('C_HIV_Stage_', seq_len(param$NoStage)),
    'C_AIDS',
    'C_Dead_D',
    'C_Dead_U',
    'C_Inf',
    'CumulIncD2Total'
  )

  # Changes made by reference
  modelResults <- cbind(
    modelResults,
    ModelAnnualNumbers(modelResults, probSurv1996, data$Prob_CD4)
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

  modelResults <- cbind(
    modelResults,
    llPosCD4_1$Result,
    llPosCD4_2$Result,
    llPosCD4_3$Result,
    llPosCD4_4$Result,
    llPos$Result,
    llAIDSPos$Result,
    llAIDS$Result
  )

  return(list(
    LLTotal = llTotal,
    ModelResults = modelResults
  ))
}

FitLLTotalDT <- function(
  p,
  probSurv1996,
  param,
  info,
  data
) {
  # CRAN checks
  Year <- NULL
  Prob_CD4 <- NULL

  VERY_LRG <- 1e+10
  # Smoothness omitted
  smooth1 <- 0
  smooth2 <- 0

  # Force numeric type to avoid conversion from integer to numeric later
  modelYears <- as.numeric(info$ModelMinYear:info$ModelMaxYear)

  param$DeltaM <- GetParamDeltaM(p, param)
  param$Theta <- GetParamTheta(p, param, info)

  derivsFunc <- GetDerivsFuncXptr('CountModel')
  res <- odeintLoop(modelYears, param, info, derivsFunc)

  modelResults <- res[['ModelResults']]
  minLambda <- res[['MinLambda']]

  modelResults <- as.data.table(modelResults)
  modelResults[, Year := modelYears[-length(modelYears)]]
  setnames(modelResults,
           c('PrimInf',
             paste0('Undiagnosed_', seq_len(param$NoStage)),
             paste0('Diagnosed_', seq_len(param$NoStage)),
             paste0('C_HIV_Stage_', seq_len(param$NoStage)),
             'C_AIDS',
             'C_Dead_D',
             'C_Dead_U',
             'C_Inf',
             'CumulIncD2Total',
             'Year'))

  # Changes made by reference
  ModelAnnualNumbersDT(modelResults, probSurv1996, data$Prob_CD4)

  if (minLambda < 0) {
    lambdaPenalty <- VERY_LRG
  } else {
    lambdaPenalty <- 0
  }

  llTotal <-
    lambdaPenalty +
    FitLLPosCD4DT(modelResults, group = 1, info, data, param) +
    FitLLPosCD4DT(modelResults, group = 2, info, data, param) +
    FitLLPosCD4DT(modelResults, group = 3, info, data, param) +
    FitLLPosCD4DT(modelResults, group = 4, info, data, param) +
    FitLLPosDT(modelResults, data, info, param) +
    FitLLAIDSPosDT(modelResults, info, data, param) +
    FitLLAIDSDT(modelResults, info, data, param) +
    param$Smoothing1 * smooth1 +
    param$Smoothing2 * smooth2 +
    sum(p[1:3] > 2) * VERY_LRG

  return(list(
    LLTotal = llTotal,
    ModelResults = modelResults
  ))
}
