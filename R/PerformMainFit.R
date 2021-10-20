#' PerformMainFit
#'
#' Perform main fit
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param param List of parameters. Optional. Default = NULL.
#' @param info List of parameters. Optional. Default = NULL.
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param maxRunTime Maximum allowed run time as \code{difftime} object. Fit will be assumed to not
#'   converging when exceeding this time. If NULL then no time out. Optional.
#'   Default = as.difftime(Inf, units = 'secs').
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param algorithm Name of optimization algorithm from package \code{nloptr} to use for bootstrap
#'   iterations. Default = 'NLOPT_LN_BOBYQA'
#' @param attemptSimplify Logical indicating to attempting simplifying the model to fit it a more
#'   complex model did not converge. The simplification is achieved by reducing the number of knots
#'   in the spline. Optional. Default = TRUE.
#' @param verbose Logical indicating to print detailed info during fitting. Optional. If missing
#'   then the value of \code{context$Settings$Verbose} is used.
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
  maxRunTime = as.difftime(Inf, units = 'secs'),
  ctol = 1e-6,
  ftol = 1e-5,
  algorithm = 'NLOPT_LN_BOBYQA',
  attemptSimplify = TRUE,
  verbose
) {
  startTime <- Sys.time()

  if (missing(verbose)) {
    verbose <- context$Settings$Verbose
  }

  dataMatrix <- as.matrix(data)

  if (is.null(info) || is.null(param)) {
    runType <- 'MAIN'
    info <- GetInfoList(context)
    param <- GetParamList(context, info)
  } else {
    runType <- 'MAIN_WITH_INIT'
  }
  probSurv1996 <- GetProvSurv96(param, info)

  tmpModelFitDist <- info$ModelFitDist
  info$ModelFitDist <- 'POISSON'
  if (tmpModelFitDist != info$ModelFitDist) {
    PrintAlert(
      'Input distribution is set to {.val {tmpModelFitDist}}.',
      'This is overridden to {.val {info$ModelFitDist}} for the fit.',
      verbose = verbose
    )
  }

  # ------------------------------------------------------------------------------------------------
  converged <- FALSE
  maxAttempts <- ifelse(attemptSimplify, Inf, 1)
  attempt <- 0L
  while (!converged && info$ModelNoKnots > 0 && attempt < maxAttempts) {
    attempt <- attempt + 1

    # AutoThetaFix ---------------------------------------------------------------------------------
    if (!info$FullData && info$SplineType == 'B-SPLINE') {
      PrintAlert('Searching for optimal number of fixed theta parameters started.')

      # Set initial number of splines with theta = 0 when doing automated search;
      # loop starts at the first knot not equal to the start year of calculations
      idxs <- seq(info$ModelNoKnots) + info$SplineOrder
      sel <- info$MyKnots[idxs] + 5 < info$FitMinYear
      if (any(sel)) {
        param$NoThetaFixInit <- max(idxs[sel]) - info$SplineOrder
      }

      # Search starts by holding the first theta fixed
      param$NoThetaFix <- as.integer(info$StartIncZero)
      cat('\n')
      PrintAlert('Number of fixed theta parameters: {.val {param$NoThetaFix}}')

      param <- UpdateThetaParams(info, param)

      res <- EstimateParameters(
        runType = runType,
        probSurv1996, param, info, dataMatrix,
        maxNoFit, maxRunTime, ctol, ftol, verbose = verbose
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
      while (llNew < (llOld + param$ChiSqDiff) && param$NoThetaFix <= nThetaFixMax) {
        param$NoThetaFix <- param$NoThetaFix + 1
        cat('\n')
        PrintAlert('Number of fixed theta parameters: {.val {param$NoThetaFix}}')

        param <- UpdateThetaParams(info, param)

        res <- EstimateParameters(
          runType = runType,
          probSurv1996, param, info, dataMatrix,
          maxNoFit, maxRunTime, ctol, ftol, verbose = verbose
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

      PrintAlert(
        'Searching for optimal number of fixed theta parameters finished: {.val {param$NoThetaFix}}'
      )
    }

    nTheta <- 100L
    while (nTheta != param$NoTheta) {
      cat('\n')
      PrintAlert('Number of spline weights to estimate: {.val {param$NoTheta}}')

      nTheta <- param$NoTheta

      res <- EstimateParameters(
        runType = runType, probSurv1996, param, info, dataMatrix, maxNoFit, maxRunTime, ctol, ftol,
        verbose = verbose
      )

      p <- res$P
      param <- res$Param
      info <- res$Info

      selSmallTheta <- abs(param$Theta) < 1
      param$ThetaP[selSmallTheta] <- 0
      param$Theta[selSmallTheta] <- 0
      param$NoTheta <- sum(param$ThetaP)
      param$ThetaF <- param$Theta[param$ThetaP != 0]
      PrintAlert(
        'Number of spline weights estimated after fixing small thetas to 0: {.val {param$NoTheta}}'
      )

      iterResults <- res$IterResults
      lastResults <- res$IterResults[[length(res$IterResults)]]

      # Recalculate LLTotal after fixing small thetas
      resNew <- FitLLTotal(p, probSurv1996, param, info, dataMatrix)
      converged <- res$Converged && abs(resNew$LLTotal - lastResults$LLTotal) <= ctol
    }

    cat('\n')
    if (!converged) {
      PrintAlert('Fit did NOT converge', type = 'danger')

      if (attempt < maxAttempts) {
        context$Parameters$INCIDENCE$ModelNoKnots <- context$Parameters$INCIDENCE$ModelNoKnots - 1
        info <- GetInfoList(context)
        info$ModelFitDist <- 'POISSON'
        param <- GetParamList(context, info)
        probSurv1996 <- GetProvSurv96(param, info)
      }
    } else {
      PrintAlert('Fit converged', type = 'success')
    }
  }

  PrintBullets(
    c(
      sprintf(
        ' beta[%2s]: %s',
        as.character(seq_len(param$NoDelta)),
        formatC(
          param$Beta[seq_len(param$NoDelta)],
          format = 'f',
          width = 15,
          digits = 6
        )
      ),
      sprintf(
        'theta[%2s]: %s %s',
        as.character(seq_along(param$Theta)),
        formatC(
          param$Theta,
          format = 'f',
          width = 15,
          digits = 6
        ),
        ifelse(param$ThetaP, '', '(FIXED)')
      )
    ),
    verbose = verbose
  )

  info$ModelFitDist <- tmpModelFitDist
  # Estimate overdispersion for negative binomial
  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    PrintAlert(
      'Estimating overdispersion type {.val {info$OverdisperionType}}',
      verbose = verbose
    )

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
        'Overdispersion: AIDS = {.val {param$RDispAIDS}}, Rest = {.val {param$RDispRest}}',
        verbose = verbose
      )
    }
  }

  res <- FitLLTotal(p, probSurv1996, param, info, dataMatrix)
  modelResults <- as.data.table(res$ModelResults)
  statRes <- FitStatistics(modelResults, info, data, param)

  countResults <- ModelCountResults(modelResults, info, param)
  timeResults <- ModelTimeResults(modelResults$Year, info, param)
  mainOutputs <- ModelOutputs(modelResults, countResults, timeResults, info, param, data)

  runTime <- Sys.time() - startTime

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
    MainOutputs = mainOutputs,
    RunTime = runTime
  ))
}
