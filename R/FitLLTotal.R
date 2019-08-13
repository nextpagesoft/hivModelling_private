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
  hMin <- 0
  h1 <- 0.02
  eps <- 0.0001
  bitSml <- 1e-6
  # Smoothness omitted
  smooth1 <- 0
  smooth2 <- 0

  modelYears <- info$ModelMinYear:info$ModelMaxYear

  param$DeltaM <- GetParamDeltaM(p, param)
  param$Theta <- GetParamTheta(p, param, info)

  ystart <- rep(0, param$NoEq)

  modelResults <- matrix(0, info$ModelNoYears - 1, param$NoEq)

  minLambda <- VERY_LRG

  for (i in seq_len(info$ModelNoYears - 1)) {
    res <- odeint(ystart,
                  nVar = param$NoEq,
                  x1 = modelYears[i] + bitSml,
                  x2 = modelYears[i + 1] - bitSml,
                  eps,
                  h1,
                  hMin,
                  param,
                  info,
                  minYear = info$ModelMinYear,
                  maxYear = info$ModelMaxYear,
                  derivsFuncName = 'derivsMainFunc')
    ystart <- res$YStart
    minLambda <- min(minLambda, res$MinLambda)
    modelResults[i, ] <- ystart
  }

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
  probCD4 <- data$Prob_CD4[-nrow(data)]
  ModelAnnualNumbers(modelResults, probSurv1996, probCD4)

  lambdaPenalty <- ifelse(minLambda < 0,
                          VERY_LRG,
                          0)
  llTotal <-
    lambdaPenalty +
    FitLLPosCD4(modelResults, group = 1, info, data, param) +
    FitLLPosCD4(modelResults, group = 2, info, data, param) +
    FitLLPosCD4(modelResults, group = 3, info, data, param) +
    FitLLPosCD4(modelResults, group = 4, info, data, param) +
    FitLLPos(modelResults, data, info, param) +
    FitLLAIDSPos(modelResults, info, data, param) +
    FitLLAIDS(modelResults, info, data, param) +
    param$Smoothing1 * smooth1 +
    param$Smoothing2 * smooth2 +
    sum(p[1:3] > 2) * VERY_LRG

  return(list(
    LLTotal = llTotal,
    ModelResults = modelResults
  ))
}
