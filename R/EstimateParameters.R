#' EstimateParameters
#'
#' Estimate parameters
#'
#' @param runType String indicating type of run, either 'MAIN' or 'BOOTSTRAP'
#' @param probSurv1996  Test
#' @param param List of parameters. Required.
#' @param info List of parameters. Required.#'
#' @param data Input data as data.table. Required.
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param algorithm Name of optimization algorithm from package \code{nloptr} to use for bootstrap
#'   iterations. Default = 'NLOPT_LN_BOBYQA'
#' @param verbose Logical indicating to print detailed info during fitting. Optional.
#'   Default = \code{TRUE}
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' EstimateParameters(runType, probSurv1996, param, info, data, maxNoFit, ctol, ftol, verbose)
#' }
#'
#' @export
EstimateParameters <- function(
  runType,
  probSurv1996,
  param,
  info,
  data,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  algorithm = 'NLOPT_LN_BOBYQA',
  verbose = TRUE
) {
  OptimFunc <- function(p) {
    return(FitLLTotal(p, probSurv1996, param, info, data, detailedResults = FALSE))
  }

  # Number of parameters in the fit:
  #  param$NoDelta - number of diagnosis rates
  #  param$NoTheta - number of spline parameters
  nParam <- param$NoDelta + param$NoTheta
  pParam <- rep(0, nParam)
  p <- rep(0, nParam)

  # Maximum number of iterations
  iterResults <- list()
  llMin <- 1e+10
  iter <- 1

  totalStartTime <- Sys.time()
  algType <- 'SCALE'
  processId <- StartProcess(
    'Iteration {.val {sprintf("%02d", iter)}}: {.emph {.val {algType}}}',
    verbose = verbose
  )
  if (runType == 'MAIN') {
    # Step 1 : determine the scale of the parameters
    defNoCD4 <- param$NoStage - 1
    iMax <- 5
    jMax <- 10
    for (i in seq_len(iMax)) {
      # Set delta1 to delta4 in the first time interval (range: 0.05 to 0.05*iMax)
      beta <- rep(i * 0.05, defNoCD4)
      # Extra contribution to delta4
      beta[defNoCD4] <- beta[defNoCD4] + 0.4
      # Keep delta's constant over time
      if (param$NoDelta > defNoCD4) {
        beta[(defNoCD4 + 1):param$NoDelta] <- 0
      }
      beta <- beta[seq_len(param$NoDelta)]

      for (j in seq_len(jMax + 1) - 1) {
        # Assume all theta's the same (range: 1 to 10^j_max)
        thetaF <- rep((j + 1) * 10^j, param$NoTheta)
        p <- GetParameterVector(beta, thetaF)
        llTotal <- OptimFunc(p)

        if (llTotal < llMin) {
          llMin <- llTotal
          pParam <- p
        }
      }
    }
    iterResults[[iter]] <- list(
      LLTotal = llMin,
      P = pParam
    )
  } else if (runType %in% c('MAIN_WITH_INIT', 'BOOTSTRAP')) {
    beta <- param$Beta
    thetaF <- param$ThetaF
    pParam <- GetParameterVector(beta, thetaF)
    llTotal <- OptimFunc(pParam)
    iterResults[[iter]] <- list(
      P = pParam,
      LLTotal = llTotal
    )
  } else {
    stop('EstimateParameters: Unsupported estimation run type')
  }

  EndProcess(
    processId,
    'Iteration {.val {sprintf("%02d", iter)}}: {.val {algType}}  |',
    'Run time: {.timestamp {prettyunits::pretty_dt(Sys.time() - totalStartTime)}}',
    verbose = verbose
  )

  # Stop fitting when the change in deviance is smaller than ctol.
  algType <- ifelse(runType %in% c('MAIN', 'MAIN_WITH_INIT'), 'AMOEBA', algorithm)
  llOld <- iterResults[[iter]]$LLTotal + ctol + 1
  while (
    abs(iterResults[[iter]]$LLTotal - llOld) > ctol &&
    iter < maxNoFit + 1
  ) {
    iter <- iter + 1

    startTime <- Sys.time()
    processId <- StartProcess(
      'Iteration {.val {sprintf("%02d", iter)}}: {.emph {.val {algType}}}',
      verbose = verbose
    )

    if (algType == 'AMOEBA') {
      res <- FitAmoeba(iter, ftol, nParam, pParam, probSurv1996, param, info, data)
    } else {
      # Algorithms checked:
      # NLOPT_LN_NELDERMEAD   - 55.5 sec, LLTotal = 239.5948
      # NLOPT_LN_BOBYQA       - 34.7 sec, LLTotal = 239.4752
      # NLOPT_LN_SBPLX        - very slow, interrupted
      # NLOPT_LN_COBYLA       - not converged, LLTotal = 20000000232.6356
      # NLOPT_LN_NEWUOA_BOUND - slow, reached maxNoFit, 1.977 mins, LLTotal = 244.279
      # NLOPT_LN_PRAXIS       - slow, reached maxNoFit, 6.144 mins, LLTotal = 238.501
      # NLOPT_LN_SBPLX        - 2.778879 mins, LLTotal = 235.5132
      optimRes <- nloptr::nloptr(
        pParam,
        OptimFunc,
        lb = c(rep(0, param$NoDelta), rep(-1e+4, param$NoTheta)),
        ub = c(rep(1, param$NoDelta), rep(1e+4, param$NoTheta)),
        opts = list(
          algorithm = algorithm,
          ftol_abs = ftol,
          maxeval = 50000
        )
      )

      p <- optimRes$solution
      fitRes <- FitLLTotal(p, probSurv1996, param, info, data)

      res <- modifyList(
        list(P = p),
        fitRes
      )
    }

    EndProcess(
      processId,
      'Iteration {.val {sprintf("%02d", iter)}}: {.val {algType}} |',
      'Run time: {.timestamp {prettyunits::pretty_dt(Sys.time() - startTime)}}',
      verbose = verbose
    )

    pParam <- res$P
    iterResults[[iter]] <- res
    llOld <- iterResults[[iter - 1]]$LLTotal
  }
  runTime <- Sys.time() - totalStartTime
  PrintAlert(
    'Total run time: {.timestamp {prettyunits::pretty_dt(runTime)}}',
    verbose = verbose
  )

  lastResults <- iterResults[[iter]]

  converged <- abs(lastResults$LLTotal - llOld) <= ctol
  beta[seq_len(param$NoDelta)] <- lastResults$P[seq_len(param$NoDelta)]
  thetaF <- lastResults$P[param$NoDelta + seq_len(param$NoTheta)]
  param$Theta <- GetParamTheta(lastResults$P, param, info)
  param$DeltaM <- GetParamDeltaM(lastResults$P, param)
  param$ThetaF <- thetaF
  param$Beta <- beta

  invisible(list(
    Converged = converged,
    P = lastResults$P,
    Info = info,
    Param = param,
    Data = data,
    IterResults = iterResults,
    RunTime = runTime
  ))
}
