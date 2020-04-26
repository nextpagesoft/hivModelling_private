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
  cli::cli_h1('2. Main fit')
  cli::cli_div(theme = list(span.orange = list(color = 'orange')))
  on.exit({
    cli::cli_end()
  })

  cli::cli_h2('2.1. Info')
  if (is.null(info) || is.null(param)) {
    runType <- 'MAIN'
    info <- GetInfoList(context)
    param <- GetParamList(context, info)
    cli::cli_alert_info(
      paste0(
        'Run type: {.orange ',
        runType,
        '} - all initial parameters will be determined from context')
    )
  } else {
    runType <- 'MAIN_WITH_INIT'
    cli::cli_alert_info(
      paste0(
        'Run type: {.orange ',
        runType,
        '} - all initial parameters will be determined from provided "info" and "param" objects'
      )
    )
  }
  probSurv1996 <- GetProvSurv96(param, info)

  tmpModelFitDist <- info$ModelFitDist
  info$ModelFitDist <- 'POISSON'
  if (tmpModelFitDist != info$ModelFitDist) {
    cli::cli_alert_info(
      paste0(
        'Input distribution was set to {.orange "', tmpModelFitDist, '"}. ',
        'This is overridden to {.orange "', info$ModelFitDist, '"} for the main fit.'
      )
    )
  }

  converged <- FALSE
  while (!converged) {
    nTheta <- 100
    while (nTheta != param$NoTheta) {
      cli::cli_alert_info('Number of estimated spline weights: {.orange {param$NoTheta}}')
      nTheta <- param$NoTheta

      res <- EstimateParameters(
        runType = runType,
        probSurv1996, param, info, data,
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
      cli::cli_alert_danger('Fit did NOT converge, goodness-of-fit: {.orange {lastResults$LLTotal}}')
      context$Parameters$Models$INCIDENCE$ModelNoKnots <- info$ModelNoKnots - 1
      info <- GetInfoList(context)
      param <- GetParamList(context, info)
      probSurv1996 <- GetProvSurv96(param, info)
    } else {
      cli::cli_alert_success('Fit converged, goodness-of-fit: {.orange {lastResults$LLTotal}}')
    }
  }

  cli::cli_h2('2.3. Results')

  thetas <- sprintf(
    'theta[%d]: {.orange %f} - %s',
    seq_along(param$Theta),
    param$Theta,
    ifelse(param$ThetaP, 'NOT FIXED', 'FIXED')
  )
  cli::cli_ul()
  cli::cli_li(sprintf('beta[%d]: %f', seq_len(param$NoDelta), param$Beta[seq_len(param$NoDelta)]))
  sapply(thetas, cli::cli_li)
  cli::cli_end()

  info$ModelFitDist <- tmpModelFitDist
  # Estimate overdispersion for negative binomial
  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    cli::cli_alert_info('Estimating overdispersion type ', info$OverdisperionType)

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

      cli::cli_alert_info(sprintf(
        'Overdispersion: AIDS = {.orange %f}, Rest = {.orange %f}',
        param$RDispAIDS,
        param$RDispRest)
      )
    }
  }

  res <- FitLLTotal(p, probSurv1996, param, info, data)
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
