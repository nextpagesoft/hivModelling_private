FitLLTotal <- function(
  p,
  deltaP,
  deltaM,
  theta,
  thetaP,
  noThetaFix,
  noDelta,
  modelSplineN,
  modelNoYears,
  modelYears,
  splineType,
  maxIncCorr,
  noEq,
  noStage,
  probSurv1996,
  model,
  param,
  info,
  data,
  extraResults
) {
  VERY_LRG <- 1e+10

  GetParamDeltaM <- function(p, deltaP, deltaM) {
    deltaM[as.logical(deltaP)] <- p[deltaP]
    return(deltaM)
  }

  GetParamTheta <- function(p, thetaP, theta, noDelta, modelSplineN) {
    k <- noDelta
    # i <- 1
    for (i in seq(modelSplineN)) {
      if (thetaP[i] != 0) {
        k <- k + 1
        theta[i] = p[k]
      }
    }
    return(theta)
  }

  # Fit_StuffParamWithVector
  deltaM <- GetParamDeltaM(p, deltaP, deltaM)
  theta <- GetParamTheta(p, thetaP, theta, noDelta, modelSplineN)

  # Model_Calculate
  if (splineType == 2) {
    if (maxIncCorr) {
      theta[modelSplineN] <- 2 * theta[modelSplineN - 1] - theta[modelSplineN - 2]
    }

    # B-splines : keep the first NoThetaFix spline weights zero
    theta[seq_len(noThetaFix)] <- 0
  }

  param[['Theta']] <<- theta
  param[['DeltaM']] <<- deltaM
  model$LambdaPenalty <<- 0

  ystart <- rep(0, noEq)

  hMin <- 0
  h1 <- 0.02
  eps <- 0.0001
  bitSml <- 1e-6

  modelResults <- matrix(0, modelNoYears - 1, noEq)
  # year <- 1
  for (year in seq_len(modelNoYears - 1)) {
    res <- odeint(ystart,
                  nVar = noEq,
                  x1 = modelYears[year] + bitSml,
                  x2 = modelYears[year + 1] - bitSml,
                  eps,
                  h1,
                  hMin,
                  param,
                  info)
    ystart <- res$YStart

    modelResults[year, ] <- ystart
  }

  modelResults <- as.data.table(modelResults)
  modelResults[, Year := modelYears[-length(modelYears)]]
  setnames(modelResults,
           c('PrimInf',
             paste0('Undiagnosed_', seq_len(noStage)),
             paste0('Diagnosed_', seq_len(noStage)),
             paste0('C_HIV_Stage_', seq_len(noStage)),
             'C_AIDS',
             'C_Dead_D',
             'C_Dead_U',
             'C_Inf',
             'CumulIncD2Total',
             'Year'))

  ModelAnnualNumbers(modelResults, probSurv1996, data)

  model$LLTotal <- model$LambdaPenalty

  model$LLPos <- FitLLPos(modelResults, data, info)

  model$LLPosCD4_1 <- FitLLPosCD4(modelResults, group = 1, info, extraResults)
  model$LLPosCD4_2 <- FitLLPosCD4(modelResults, group = 2, info, extraResults)
  model$LLPosCD4_3 <- FitLLPosCD4(modelResults, group = 3, info, extraResults)
  model$LLPosCD4_4 <- FitLLPosCD4(modelResults, group = 4, info, extraResults)

  model$LLTotal <- model$LLTotal +
    model$LLPosCD4_1 + model$LLPosCD4_2 + model$LLPosCD4_3 + model$LLPosCD4_4

  model$LLAIDSPos <- FitLLAIDSPos(modelResults, info, extraResults)
  model$LLAIDS <- FitLLAIDS(modelResults, info, extraResults)

  # Smoothness omitted
  model$Smooth1 <- 0
  model$Smooth2 <- 0

  model$LLTotal <- model$LLTotal +
    model$LLAIDSPos + model$LLAIDS + model$LLPos

  model$LLTotal <- model$LLTotal +
    param$Smoothing1 * model$Smooth1 + param$Smoothing2 * model$Smooth2

  # SplineType == 1 not supported

  # Severely punish clearly wrong beta's
  model$LLTotal <- model$LLTotal + sum(p[1:3] > 2) * VERY_LRG

  return(model$LLTotal)
}
