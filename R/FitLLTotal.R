FitLLTotal <- function(
  p,
  probSurv1996,
  param,
  info,
  data
) {
  # CRAN checks
  Year <- NULL

  VERY_LRG <- 1e+10
  hMin <- 0
  h1 <- 0.02
  eps <- 0.0001
  bitSml <- 1e-6
  # Smoothness omitted
  smooth1 <- 0
  smooth2 <- 0

  param$DeltaM <- GetParamDeltaM(p, param)
  param$Theta <- GetParamTheta(p, param, info)

  ystart <- rep(0, param$NoEq)

  modelResults <- matrix(0, info$ModelNoYears - 1, param$NoEq)

  minLambda <- VERY_LRG

  # i <- 10
  for (i in seq_len(info$ModelNoYears - 1)) {
    res <- odeint(ystart,
                  nVar = param$NoEq,
                  x1 = info$ModelYears[i] + bitSml,
                  x2 = info$ModelYears[i + 1] - bitSml,
                  eps,
                  h1,
                  hMin,
                  param,
                  info)
    ystart <- res$YStart
    minLambda <- min(minLambda,
                     res$MinLambda)
    modelResults[i, ] <- ystart
  }

  modelResults <- as.data.table(modelResults)
  modelResults[, Year := info$ModelYears[-length(info$ModelYears)]]
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
  ModelAnnualNumbers(modelResults, probSurv1996, data)

  lambdaPenalty <- ifelse(minLambda < 0,
                          VERY_LRG,
                          0)
  llTotal <-
    lambdaPenalty +
    FitLLPosCD4(modelResults, group = 1, info, data) +
    FitLLPosCD4(modelResults, group = 2, info, data) +
    FitLLPosCD4(modelResults, group = 3, info, data) +
    FitLLPosCD4(modelResults, group = 4, info, data) +
    FitLLPos(modelResults, data, info) +
    FitLLAIDSPos(modelResults, info, data) +
    FitLLAIDS(modelResults, info, data) +
    param$Smoothing1 * smooth1 +
    param$Smoothing2 * smooth2 +
    sum(p[1:3] > 2) * VERY_LRG

  return(list(
    LLTotal = llTotal,
    Model = modelResults
  ))
}
