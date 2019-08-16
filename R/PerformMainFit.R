#' PerformMainFit
#'
#' Perform main fit
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
#' Results list object
#'
#' @examples
#' \dontrun{
#' PerformMainFit(context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose = TRUE)
#' }
#'
#' @export
PerformMainFit <- function(
  context,
  data,
  maxNoFit = 30L,
  ctol = 1e-6,
  ftol = 1e-5,
  ...
) {
  param <- GetParamList(context)
  info <- GetInfoList(context)
  probSurv1996 <- GetProvSurv96(param, info)

  tmpModelFitDist <- info$ModelFitDist
  info$ModelFitDist <- 'POISSON'
  if (tmpModelFitDist != info$ModelFitDist) {
    message('Input distribution was set to "', tmpModelFitDist, '. ',
            'This is overridden to "', info$ModelFitDist, '" for the main fit.')
  }

  nTheta <- 100L
  while (nTheta != param$NoTheta) {
    message(sprintf('Number of estimated spline weights: %d', param$NoTheta))
    nTheta <- param$NoTheta

    res <- EstimateParameters(
      runType = 'MAIN', mainResults = NULL,
      probSurv1996, param, info, data, maxNoFit, ctol, ftol, ...
    )

    p <- res$P
    converged <- res$Converged
    beta <- res$Beta
    thetaF <- res$ThetaF
    param <- res$Param
    info <- res$Info
    iterResults <- res$IterResults
    lastResults <- res$IterResults[[length(res$IterResults)]]
  }

  if (converged) {
    message(sprintf('Fit converged with goodness-of-fit: %f\n', lastResults$LLTotal))
  }

  message(sprintf('beta[%d]: %f\n', seq_len(param$NoDelta), beta[seq_len(param$NoDelta)]))
  message(sprintf('theta[%d]: %f\n', seq_len(param$NoTheta), thetaF[seq_len(param$NoTheta)]))

  info$ModelFitDist <- tmpModelFitDist
  # Estimate overdispersion for negative binomial
  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    message('Estimating overdispersion type ', info$OverdisperionType)

    rMin <- 1.0
    rMax <- 100000

    if (info$OverdisperionType != 2) {
      stop(sprintf('Overdisperion type %d is not supported', info$OverdisperionType))
    } else if (info$OverdisperionType == 2) {
      extraArgs <- list(
        Type = 0,
        ModelResults = lastResults$ModelResults,
        Data = data,
        Info = info,
        Param = param
      )
      # Use base R function 'uniroot' instead of zbrent
      param$RDispAIDS <- zbrent(FitLLrAIDS, rMin, rMax, ftol, extraArgs)
      param$RDispRest <- zbrent(FitLLrRest, rMin, rMax, ftol, extraArgs)

      message(sprintf('Overdispersion: AIDS = %f, Rest = %f', param$RDispAIDS, param$RDispRest))
    }
  }

  res <- FitLLTotal(p, probSurv1996, param, info, data)
  modelResults <- res$ModelResults
  statRes <- FitStatistics(modelResults, info, data, param)

  countResults <- ModelCountResults(modelResults, info, param)
  timeResults <- ModelTimeResults(modelResults, info, param)
  mainOutputs <- ModelOutputs(modelResults, countResults, timeResults, info, param, data)

  return(list(
    Converged = converged,
    P = p,
    Beta = beta,
    Theta = param$theta,
    ThetaF = thetaF,
    DeltaM = param$DeltaM,
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
