#' PerformMainFit
#'
#' Perform main fit
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param param List of parameters. Optional. Default = NULL.
#' @param info List of parameters. Optional. Default = NULL.
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param algorithm Name of optimization algorithm from package \code{nloptr} to use for bootstrap
#'   iterations. Default = 'NLOPT_LN_BOBYQA'
#' @param verbose Logical indicating to print detailed info during fitting. Optional.
#'   Default = \code{FALSE}
#'
#' @return
#' Results list object
#'
#' @examples
#' \dontrun{
#' PerformMainFit(
#'   context, data, param, info, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose = TRUE
#' )
#' }
#'
#' @export
PerformMainFit <- function(
  context,
  data,
  param = NULL,
  info = NULL,
  maxNoFit = 30L,
  ctol = 1e-6,
  ftol = 1e-5,
  algorithm = 'NLOPT_LN_BOBYQA',
  verbose = FALSE
) {
  PrintH1('2. Main fit')

  PrintH2('2.1. Info')
  if (is.null(info) || is.null(param)) {
    runType <- 'MAIN'
    info <- GetInfoList(context)
    param <- GetParamList(context, info)
    PrintAlert(
      'Run type: {.val {runType}}',
      '- all initial parameters will be determined from object {.var context}'
    )
  } else {
    runType <- 'MAIN_WITH_INIT'
    PrintAlert(
      'Run type: {.val {runType}}',
      '- all initial parameters will be determined from objects',
      '{.var info} and {.var param}'
    )
  }
  probSurv1996 <- GetProvSurv96(param, info)
  dataMatrix <- as.matrix(data)

  tmpModelFitDist <- info$ModelFitDist
  info$ModelFitDist <- 'POISSON'
  if (tmpModelFitDist != info$ModelFitDist) {
    PrintAlert(
      'Input distribution was set to {.val {tmpModelFitDist}}.',
      'This is overridden to {.val {info$ModelFitDist}} for the main fit.'
    )
  }

  PrintH2('2.2. Iterations')

  # AutoThetaFix -----------------------------------------------------------------------------------
  if (!info$FullData && info$SplineType == 'B-SPLINE') {
    # Set initial number of splines with theta = 0 when doing automated search;
    # loop starts at the first knot not equal to the start year of calculations
    idxs <- seq(info$ModelNoKnots) + info$SplineOrder
    sel <- info$MyKnots[idxs] + 5 < info$FitMinYear
    if (any(sel)) {
      param$NoThetaFixInit <- max(idxs[sel]) - info$SplineOrder
    }

    # Search starts by holding the first theta fixed
    param$NoThetaFix <- as.integer(info$StartIncZero)

    param <- UpdateThetaParams(info, param)

    res <- EstimateParameters(
      runType = runType,
      probSurv1996, param, info, dataMatrix,
      maxNoFit, ctol, ftol, verbose
    )

    res <- FitLLTotal(res$P, probSurv1996, param, info, dataMatrix)
    modelResults <- as.data.table(res$ModelResults)
    statRes <- FitStatistics(modelResults, info, data, param)
    llNew <- statRes$LL_Poisson
    llBest <- statRes$LL_Poisson
    llOld <- 1e+7

    nThetaFixBest <- param$NoThetaFix
    nThetaFixMax <- info$ModelSplineN - (as.integer(info$MaxIncCorr) + 1)
    # Increase the number of fixed spline weights until the fit gets too bad

    while (llNew < (llOld + param$ChiSqDiff) && param$NoThetaFix <= nThetaFixMax)
    {
      param$NoThetaFix <- param$NoThetaFix + 1
      param <- UpdateThetaParams(info, param)

      res <- EstimateParameters(
        runType = runType,
        probSurv1996, param, info, dataMatrix,
        maxNoFit, ctol, ftol, verbose
      )

      res <- FitLLTotal(res$P, probSurv1996, param, info, dataMatrix)
      modelResults <- as.data.table(res$ModelResults)
      statRes <- FitStatistics(modelResults, info, data, param)
      llNew <- statRes$LL_Poisson

      if (llNew < (llOld + param$ChiSqDiff)) {
        nThetaFixBest <- param$NoThetaFix
        llBest <- llNew
        llOld <- llNew
      }
    }

    param$NoThetaFix <- nThetaFixBest
    param <- UpdateThetaParams(info, param)
  }

  # ------------------------------------------------------------------------------------------------
  converged <- FALSE
  while (!converged) {
    nTheta <- 100
    while (nTheta != param$NoTheta) {
      PrintAlert('Number of spline weights to estimate: {.val {param$NoTheta}}')
      nTheta <- param$NoTheta

      res <- EstimateParameters(
        runType = runType,
        probSurv1996, param, info, dataMatrix,
        maxNoFit, ctol, ftol, verbose
      )

      p <- res$P
      converged <- res$Converged
      param <- res$Param
      info <- res$Info

      selSmallTheta <- abs(param$Theta) < 1
      param$ThetaP[selSmallTheta] <- 0
      param$Theta[selSmallTheta] <- 0
      param$NoTheta <- sum(param$ThetaP)

      iterResults <- res$IterResults
      lastResults <- res$IterResults[[length(res$IterResults)]]
    }

    if (!converged) {
      PrintAlert(
        'Fit did NOT converge, goodness-of-fit: {.val {lastResults$LLTotal}}',
        type = 'danger'
      )
      context$Parameters$Models$INCIDENCE$ModelNoKnots <- info$ModelNoKnots - 1
      info <- GetInfoList(context)
      param <- GetParamList(context, info)
      probSurv1996 <- GetProvSurv96(param, info)
    } else {
      PrintAlert(
        'Fit converged, goodness-of-fit: {.val {lastResults$LLTotal}}',
        type = 'success'
      )
    }
  }

  PrintH2('2.3. Results')

  PrintBullets(c(
    sprintf('beta[%d]: {.val %f}', seq_len(param$NoDelta), param$Beta[seq_len(param$NoDelta)]),
    sprintf(
      'theta[%d]: {.val %f} - %s', seq_along(param$Theta), param$Theta,
      ifelse(param$ThetaP, 'NOT FIXED', 'FIXED')
    )
  ))

  info$ModelFitDist <- tmpModelFitDist
  # Estimate overdispersion for negative binomial
  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    PrintAlert('Estimating overdispersion type {.val {info$OverdisperionType}}')

    rMin <- 1.0
    rMax <- 100000

    if (info$OverdisperionType != 2) {
      stop(sprintf('Overdisperion type %d is not supported', info$OverdisperionType))
    } else if (info$OverdisperionType == 2) {
      extraArgs <- list(
        Type = 0,
        ModelResults = lastResults$ModelResults,
        Data = dataMatrix,
        Info = info,
        Param = param
      )
      # Use base R function 'uniroot' instead of zbrent
      param$RDispAIDS <- Zbrent(FitLLrAIDS, rMin, rMax, ftol, extraArgs)
      param$RDispRest <- Zbrent(FitLLrRest, rMin, rMax, ftol, extraArgs)

      PrintAlert(
        'Overdispersion: AIDS = {.val {param$RDispAIDS}}, Rest = {.val {param$RDispRest}}'
      )
    }
  }

  res <- FitLLTotal(p, probSurv1996, param, info, dataMatrix)
  modelResults <- as.data.table(res$ModelResults)
  statRes <- FitStatistics(modelResults, info, data, param)

  countResults <- ModelCountResults(modelResults, info, param)
  timeResults <- ModelTimeResults(modelResults, info, param)
  mainOutputs <- ModelOutputs(modelResults, countResults, timeResults, info, param, data)

  invisible(list(
    Converged = converged,
    P = p,
    Info = info,
    Param = param,
    Statistics = statRes,
    IterResults = iterResults,
    ModelResults = modelResults,
    CountResults = countResults,
    TimeResults = timeResults,
    MainOutputs = mainOutputs
  ))
}
