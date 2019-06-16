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
  GetParamList <- function(context) {
    incidenceParams <- context$Parameters$Models$INCIDENCE

    param <- list(
      NoStage = incidenceParams$NoStage,
      FInit = incidenceParams$FInit,
      NoTime = incidenceParams$NoTime,
      Mu = incidenceParams$Mu,
      AlphaP = incidenceParams$AlphaP,
      Qoppa = incidenceParams$Qoppa,
      Delta4Fac = incidenceParams$Delta4Fac,
      DeltaAIDS = incidenceParams$DeltaAIDS,
      Tc = incidenceParams$Tc,
      NoDelta = incidenceParams$NoDelta,
      NoTheta = incidenceParams$NoTheta,
      NoThetaFix = incidenceParams$NoThetaFix,
      AutoThetaFix = ifelse(incidenceParams$FullData, 0L, 1L),
      Smoothing1 = incidenceParams$Smoothing1,
      Smoothing2 = incidenceParams$Smoothing2
    )

    deltaM <- matrix(0, param$NoStage, param$NoTime)
    deltaM[param$NoStage, ] <- param$DeltaAIDS
    deltaM[1:4, 2] <- 0.2

    theta <- rep(0, param$NoTheta + 2)

    deltaP <- matrix(0, param$NoStage, param$NoTime)
    deltaP[1:4, 2] <- 1

    thetaP <- rep(0, param$NoTheta + 2)
    thetaP[2:(param$NoTheta + 1)] <- 1

    param[['NoEq']] <-
      1 + param$NoStage + param$NoStage + param$NoStage + 1 + 1 + 1 + 1 + 1
    param[['DeltaP']] <- deltaP
    param[['ThetaP']] <- thetaP
    param[['DeltaM']] <- deltaM
    param[['Theta']] <- theta
    param[['NoStageTot']] <- param$NoStage + 1

    return(param)
  }

  GetInfoList <- function(context) {
    incidenceParams <- context$Parameters$Models$INCIDENCE

    info <- list(
      Country = incidenceParams$Country,
      ModelMinYear = incidenceParams$ModelMinYear,
      ModelMaxYear = incidenceParams$ModelMaxYear,
      FitPosMinYear = incidenceParams$FitPosMinYear,
      FitPosMaxYear = incidenceParams$FitPosMaxYear,
      FitPosCD4MinYear = incidenceParams$FitPosCD4MinYear,
      FitPosCD4MaxYear = incidenceParams$FitPosCD4MaxYear,
      FitAIDSPosMinYear = incidenceParams$FitAIDSPosMinYear,
      FitAIDSPosMaxYear = incidenceParams$FitAIDSPosMaxYear,
      FitAIDSMinYear = incidenceParams$FitAIDSMinYear,
      FitAIDSMaxYear = incidenceParams$FitAIDSMaxYear,

      ModelFitDist = 'POISSON',

      ModelNoKnots = incidenceParams$ModelNoKnots,

      SplineType = incidenceParams$SplineType,
      SplineOrder = incidenceParams$SplineOrder,
      MaxIncCorr = incidenceParams$MaxIncCorr
    )

    message('Input distribution was set to "', incidenceParams$FitDistribution, '".\n',
            'This is overridden to "POISSON".')

    info[['ModelNoYears']] <- info$ModelMaxYear - info$ModelMinYear + 1
    info[['ModelSplineN']] <- info$ModelNoKnots + info$SplineOrder

    # Param_Knots
    knotsDistance <-
      (info$ModelMaxYear - info$ModelMinYear) / (info$ModelNoKnots + 1)

    knots <- matrix(0,
                    info$SplineOrder + 1,
                    info$ModelNoKnots + 2 * (info$SplineOrder + 1))
    myKnots <- NULL
    for (k in seq_len(info$SplineOrder + 1)) {
      knots[k, 1:k] <- info$ModelMinYear
      knots[k, (k + 1):(k + info$ModelNoKnots)] <-
        info$ModelMinYear +
        ((k + 1):(k + info$ModelNoKnots) - k) * knotsDistance
      knots[k, (k + info$ModelNoKnots + 1):(info$ModelNoKnots + 2 * k)] <-
        info$ModelMaxYear + VERY_SML

      if (k == info$SplineOrder) {
        myKnots <- knots[k, seq_len(2 * info$SplineOrder + info$ModelNoKnots)]
      }
    }

    info[['Knots']] <- knots
    info[['MyKnots']] <- myKnots

    return(info)
  }

  GetParameterVector <- function(beta, thetaF, param) {
    p <- rep(0, param$NoDelta + param$NoTheta)
    p[seq(param$NoDelta)] <- beta[seq(param$NoDelta)]
    p[param$NoDelta + seq(param$NoTheta)] <- thetaF[seq(param$NoTheta)]
    return(p)
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
