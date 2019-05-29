#' PerformMainFit
#'
#' Description
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param ... Additional arguments passed to amoeba function. Optional.
#'
#' @return
#' List of amoeba results
#'
#' @examples
#' \dontrun{
#' PerformMainFit(context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5)
#' }
#'
#' @export
PerformMainFit <- function(
  context,
  data,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  ...
) {
  # Constants ------------------------------------------------------------------
  VERY_SML <- 1e-20

  # Helper functions -----------------------------------------------------------
  GetParameterVector <- function(beta, thetaF, param) {
    p <- rep(0, param$NoDelta + param$NoTheta)
    p[seq(param$NoDelta)] <- beta[seq(param$NoDelta)]
    p[param$NoDelta + seq(param$NoTheta)] <- thetaF[seq(param$NoTheta)]
    return(p)
  }

  GetParamList <- function(context) {
    param <- list(
      # Number of disease stages
      NoStage = 5,
      # Number of time intervals diagnosis matrix
      NoTime = 8,
      # Background mortality
      Mu = 0,
      AlphaP = 1.0/(2.90/12),
      Qoppa = c(1/6.37, 1/2.86, 1/3.54, 1/2.3, 0.529101),
      Delta4Fac = 0,
      DeltaAIDS = 12,
      # Time intervals
      Tc = c(1980, 1984, 1984, 1996, 2000, 2005, 2010, 2017),
      NoDelta = 1,
      NoTheta = 8,
      NoThetaFix = 0,
      Smoothing1 = 0,
      Smoothing2 = 0
    )

    fInit <- c(0.58, 0.23, 0.16, 0.03, 0)
    fInit[1] <- 1 - sum(fInit[-1])
    param[['FInit']] <- fInit
    param[['NoEq']] <- 1 + param$NoStage + param$NoStage + param$NoStage + 1 + 1 + 1 + 1 + 1

    deltaM <- matrix(0, param$NoStage, param$NoTime)
    deltaM[param$NoStage, ] <- param$DeltaAIDS
    deltaM[1:4, 2] <- 0.2

    theta <- rep(0, param$NoTheta + 2)

    deltaP <- matrix(0, param$NoStage, param$NoTime)
    deltaP[1:4, 2] <- 1

    thetaP <- rep(0, param$NoTheta + 2)
    thetaP[2:(param$NoTheta + 1)] <- 1

    param[['DeltaP']] <- deltaP
    param[['ThetaP']] <- thetaP
    param[['DeltaM']] <- deltaM
    param[['Theta']] <- theta

    param[['NoStageTot']] <- param$NoStage + 1

    return(param)
  }

  GetInfoList <- function(context) {
    info <- list(
      Country = context$Parameters$Models$INCIDENCE$Country,
      # Rate of progression to AIDS through stages of CD4
      #   type = 2 : Lodi et al, CID 2011, 53:817-825
      #   CASCADE Lancet 2000, 355:1131-37, Table 2.
      #   Cori et al, PLoS One 2014, 9(1):e84511, supplement
      ModelCD4Rate = 2,
      # Number of time intervals diagnosis matrix
      ModelNoTime = 7,
      ModelMinYear = 1980,
      ModelMaxYear = 2017,
      FitPosMinYear = 1979,
      FitPosMaxYear = 1979,
      FitPosCD4MinYear = 1984,
      FitPosCD4MaxYear = 2016,
      FitAIDSPosMinYear = 1996,
      FitAIDSPosMaxYear = 2016,
      FitAIDSMinYear = 1980,
      FitAIDSMaxYear = 1995,
      ModelFitDist = 1, # Poisson
      ModelNoKnots = 6,
      # B-splines : smooth incidence curve at the end of the observation interval
      SplineType = 2,
      ModelSplOrder = 4,
      # Correction for incidence at end of observation interval (1=yes; 0=no) by
      # extending spline base beyond the maximum year and fixing the parameter
      # associated with the last spline function to 0
      MaxIncCorr = TRUE,
      StartRandom = FALSE
    )

    info[['TmpMinYear']] <- info$ModelMinYear
    info[['TmpMaxYear']] <- info$ModelMaxYear
    info[['ModelYears']] <- info$ModelMinYear:info$ModelMaxYear
    info[['ModelNoYears']] <- info$ModelMaxYear - info$ModelMinYear + 1
    info[['ModelSplineN']] <- info$ModelNoKnots + info$ModelSplOrder

    # Param_Knots
    if (info$SplineType != 3) {
      info[['KnotsDistance']] <- (info$ModelMaxYear - info$ModelMinYear) / (info$ModelNoKnots + 1)
    } else {
      info[['KnotsDistance']] <- (info$ModelMaxYear + 5 - info$ModelMinYear) / (info$ModelNoKnots + 2 * info$ModelSplOrder - 1)
    }

    knots <- matrix(0, info$ModelSplOrder + 1, info$ModelNoKnots + 2 * (info$ModelSplOrder + 1))
    myKnots <- NULL
    for (k in seq_len(info$ModelSplOrder + 1)) {
      if (info$SplineType != 3) {
        knots[k, 1:k] <- info$ModelMinYear
        knots[k, (k + 1):(k + info$ModelNoKnots)] <- info$ModelMinYear + ((k + 1):(k + info$ModelNoKnots) - k) * info$KnotsDistance
        knots[k, (k + info$ModelNoKnots + 1):(info$ModelNoKnots + 2 * k)] <- info$ModelMaxYear + VERY_SML
      } else {
        stop('Param_Knots for splineType == 3 not implemented')
      }

      if (k == info$ModelSplOrder) {
        myKnots <- knots[k, seq_len(2 * info$ModelSplOrder + info$ModelNoKnots)]
      }
    }

    info[['Knots']] <- knots
    info[['MyKnots']] <- myKnots

    return(info)
  }

  # Code -----------------------------------------------------------------------

  param <- GetParamList(context)
  info <- GetInfoList(context)

  probSurv1996 <- GetProvSurv96(
    info$Country,
    param$NoStage,
    param$Qoppa,
    info$ModelMinYear,
    info$ModelNoYears)

  # Number of parameters in the fit:
  #  param$NoDelta - number of diagnosis rates
  #  param$NoTheta - number of spline parameters
  nParam <- param$NoDelta + param$NoTheta
  pParam <- rep(0, nParam)
  p <- rep(0, nParam)

  # Maximum number of iterations
  allResults <- list()
  llMin <- 1.0e+10
  iter <- 1

  # Step 1 : determine the scale of the parameters
  defNoCD4 <- 4
  iMax <- 5
  jMax <- 10
  # i <- 1
  startTime <- Sys.time()
  message('--- Iteration ', iter, ': Scale')
  # i <- 1
  for (i in seq_len(iMax)) {
    # Set delta1 to delta4 in the first time interval (range: 0.05 to 0.05*iMax)
    beta <- rep(i * 0.05, defNoCD4)
    # Extra contribution to delta4
    beta[defNoCD4] <- beta[defNoCD4] + 0.4
    # Keep delta's constant over time
    if (param$NoDelta > defNoCD4) {
      beta[(defNoCD4 + 1):param$NoDelta] <- 0
    }

    # j <- 0
    for (j in seq_len(jMax + 1) - 1) {
      # Assume all theta's the same (range: 1 to 10^j_max)
      thetaF <- rep((j + 1) * 10^j, param$NoTheta)

      p <- GetParameterVector(beta, thetaF, param)

      res <- FitLLTotal(p, probSurv1996, param, info, data)
      ll <- res$LLTotal

      if (ll < llMin) {
        llMin <- ll
        pParam <- p
        allResults[[iter]] <- list(
          P = p,
          LLTotal = res$LLTotal,
          ModelResults = res$ModelResults
        )
      }
    }
  }
  message('  Run time: ', format(Sys.time() - startTime))

  # Fill beta and thetaF with the best fitting parameters
  beta[seq_len(param$NoDelta)] <- pParam[seq_len(param$NoDelta)]
  thetaF <- pParam[param$NoDelta + seq_len(param$NoTheta)]

  # Stop fitting when the change in deviance is smaller than ctol.
  llOld <- 0
  while (
    abs(allResults[[iter]]$LLTotal - llOld) > ctol &&
    iter < maxNoFit
  ) {
    iter <- iter + 1

    message('--- Iteration ', iter, ': Amoeba')
    startTime <- Sys.time()
    res <- FitAmoeba(ifit = iter, ftol = ftol, nParam, pParam,
                     probSurv1996,
                     param,
                     info,
                     data,
                     ...)
    message('  Run time: ', format(Sys.time() - startTime))

    pParam <- res$P
    allResults[[iter]] <- res
    llOld <- allResults[[iter - 1]]$LLTotal
  }

  return(allResults)
}
