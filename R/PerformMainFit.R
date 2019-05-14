#' PerformMainFit
#'
#' Description
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#'
#' @return
#' model data.table object
#'
#' @examples
#' \dontrun{
#' PerformMainFit(context)
#' }
#'
#' @export
PerformMainFit <- function(context, data)
{
  VERY_LRG <- 1e+10

  GetVector <- function(beta, thetaF, noDelta, noTheta) {
    p <- rep(0, noDelta + noTheta)
    p[seq(noDelta)] <- beta[seq(noDelta)]
    p[noDelta + seq(noTheta)] <- thetaF[seq(noTheta)]
    return(p)
  }

  VERY_SML <- 1e-20

  country <- context$Parameters$Models$INCIDENCE$Country

  # DEBUG

  # Number of disease stages
  noStage <- 5

  # Number of time intervals diagnosis matrix
  noTime <- 8

  # Rate of progression to AIDS through stages of CD4
  #   type = 2 : Lodi et al, CID 2011, 53:817-825
  #   CASCADE Lancet 2000, 355:1131-37, Table 2.
  #   Cori et al, PLoS One 2014, 9(1):e84511, supplement
  modelCD4Rate <- 2

  # Maximum number of iterations
  maxNoFit <- 100

  # Number of time intervals diagnosis matrix
  modelNoTime <- 7

  modelMinYear <- 1980
  modelMaxYear <- 2017
  modelYears <- modelMinYear:modelMaxYear

  modelNoYears <- modelMaxYear - modelMinYear + 1


  modelNoKnots <- 6
  modelSplOrder <- 4
  modelSplineN <- modelNoKnots + modelSplOrder

  # B-splines : smooth incidence curve at the end of the observation interval
  splineType <- 2

  # Param_Knots
  if (splineType != 3) {
    knotsDistance <- (modelMaxYear - modelMinYear) / (modelNoKnots + 1)
  } else {
    knotsDistance <- (modelMaxYear + 5 - modelMinYear) / (modelNoKnots + 2 * modelSplOrder - 1)
  }

  knots <- matrix(0, modelSplOrder + 1, modelNoKnots + 2 * (modelSplOrder + 1))
  # k <- 1
  for (k in seq_len(modelSplOrder + 1)) {
    if (splineType != 3) {
      knots[k, 1:k] <- modelMinYear
      knots[k, (k + 1):(k + modelNoKnots)] <- modelMinYear + ((k + 1):(k + modelNoKnots) - k) * knotsDistance
      knots[k, (k + modelNoKnots + 1):(modelNoKnots + 2 * k)] <- modelMaxYear + VERY_SML
    } else {
      stop('Param_Knots for splineType == 3 not implemented')
    }

    if (k == modelSplOrder) {
      myKnots <- knots[k, seq_len(2 * modelSplOrder + modelNoKnots)]
    }
  }

  # Correction for incidence at end of observation interval (1=yes; 0=no) by
  # extending spline base beyond the maximum year and fixing the parameter
  # associated with the last spline function to 0
  maxIncCorr <- TRUE

  # Background mortality
  mu <- 0

  alphaP <- 1.0/(2.90/12)

  fInit <- c(0.58, 0.23, 0.16, 0.03, 0)
  fInit[1] <- 1 - sum(fInit[-1])
  qoppa <- c(1/6.37, 1/2.86, 1/3.54, 1/2.3, 0.529101)
  delta4Fac <- 0
  deltaAIDS <- 12

  # Time intervals
  tc <- c(1980, 1984, 1984, 1996, 2000, 2005, 2010, 2017)


  # Model_GetNoEquation();
  noEq <- 1 + noStage + noStage + noStage + 1 + 1 + 1 + 1 + 1
  modelNoIter <- maxNoFit
  startRandom <- FALSE
  iRun <- 0

  # Param_Prob_Surv
  probSurv1996 <- GetProvSurv96(country, noStage, qoppa, modelMinYear, modelNoYears)

  # Number of parameters?
  noDelta <- 1
  noTheta <- 8
  nTheta <- noTheta
  noThetaFix <- 0

  # Param_Delta
  deltaM <- matrix(0, noStage, noTime)
  deltaP <- matrix(0, noStage, noTime)

  deltaP[1:4, 2] <- 1

  deltaM[noStage, ] <- deltaAIDS
  deltaM[1:4, 2] <- 0.2

  theta <- rep(0, noTheta + 2)
  thetaP <- rep(0, noTheta + 2)
  thetaP[2:(noTheta + 1)] <- 1

  # Fit_EstimateParameters
  llFinal <- rep(0, maxNoFit)

  # number of parameters in the fit:
  # param->NoDelta : number of diagnosis rates
  #  param->NoTheta : number of spline parameters
  nParam <- noDelta + noTheta

  pParam <- rep(0, nParam)

  # Fit_Initialise
  p <- rep(0, nParam)

  llMin <- 1.0e+10

  nIterMax <- 30
  iter <- 1

  param <- list(
    NoStage = noStage,
    NoStageTot = noStage + 1,
    Tc = tc,
    DeltaAIDS = deltaAIDS,
    Delta4Fac = delta4Fac,
    Theta = theta,
    DeltaM = deltaM,
    AlphaP = alphaP,
    Mu = mu,
    FInit = fInit,
    Qoppa = qoppa,
    Smoothing1 = 0,
    Smoothing2 = 0,
    NoDelta = noDelta,
    NoTheta = noTheta
  )

  info <- list(
    TmpMinYear = modelMinYear,
    TmpMaxYear = modelMaxYear,
    SplineType = splineType,
    SplineOrder = 4,
    ModelSplineN = modelSplineN,
    MyKnots = myKnots,
    FitPosMinYear = 1979,
    FitPosMaxYear = 1979,
    FitPosCD4MinYear = 1984,
    FitPosCD4MaxYear = 2016,
    FitAIDSPosMinYear = 1996,
    FitAIDSPosMaxYear = 2016,
    FitAIDSMinYear = 1980,
    FitAIDSMaxYear = 1995,
    ModelFitDist = 1 # Poisson
  )

  model <- list(
    LambdaPenalty = 0,
    LLTotal = 0,
    LLPos = 0,
    LLPosCD4_1 = 0,
    LLPosCD4_2 = 0,
    LLPosCD4_3 = 0,
    LLPosCD4_4 = 0,
    LLAIDSPos = 0,
    LLAIDS = 0,
    Smooth1 = 0,
    Smooth2 = 0
  )

  extraResults <- data.table(
    Year = modelMinYear:modelMaxYear,
    LL_PosCD4_Year_1 = 0,
    LL_PosCD4_Year_2 = 0,
    LL_PosCD4_Year_3 = 0,
    LL_PosCD4_Year_4 = 0,
    LL_AIDSPos_Year = 0
  )

  # Step 1 : determine the scale of the parameters
  noCD4 <- 4
  iMax <- 5
  jMax <- 10
  # i <- 1
  for (i in seq(iMax)) {
    # Set delta1 to delta4 in the first time interval (range: 0.05 to 0.05*i_max)
    beta <- rep(i * 0.05, noCD4)
    # Extra contribution to delta4
    beta[noCD4] <- beta[noCD4] + 0.4
    # Keep delta's constant over time
    if (noDelta > noCD4) {
      beta[(noCD4 + 1):noDelta] <- 0
    }

    # j <- 3
    for (j in seq(jMax + 1) - 1) {
      # Assume all theta's the same (range: 1 to 10^j_max)
      thetaF <- rep((j + 1) * 10^j, noTheta)

      # Fit_StuffVectorWithParam
      p <- GetVector(beta, thetaF, noDelta, noTheta)

      # Fit_LL_Total
      ll <- FitLLTotal(p, deltaP, deltaM, theta, thetaP, noThetaFix, noDelta,
                       modelSplineN, modelNoYears, modelYears, splineType,
                       maxIncCorr, noEq, noStage, probSurv1996, model, param,
                       info, data, extraResults)

      if (ll < llMin) {
        llMin <- ll
        pParam <- p
      }
    }

    # Fill beta and theta_f with the best fitting parameters
    beta[seq_len(noDelta)] <- pParam[seq_len(noDelta)]
    thetaF <- pParam[noDelta + seq_len(noTheta)]
  }

  llFinal[iter] <- llMin

  # Stop fitting when the change in deviance is smaller than ctol.
  j <- 0
  ctol <- 1e-6
  llOld <- 0
  while (
    abs(llFinal[iter] - llOld) > ctol &&
    iter < nIterMax
  ) {
    j <- j + 1
    iter <- iter + 1
    ftol <- 1e-5

    FitAmoeba(ifit = iter, ftol, nParam, pParam, param,
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
              info,
              data,
              extraResults)
  }

  return(model)
}
