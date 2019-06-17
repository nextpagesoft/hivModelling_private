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
  iterResults <- list()
  llMin <- 1.0e+10
  iter <- 1

  # Step 1 : determine the scale of the parameters
  defNoCD4 <- param$NoStage - 1
  iMax <- 5
  jMax <- 10
  startTime <- Sys.time()
  message('--- Iteration ', iter, ': Scale')
  for (i in seq_len(iMax)) {
    # Set delta1 to delta4 in the first time interval (range: 0.05 to 0.05*iMax)
    beta <- rep(i * 0.05, defNoCD4)
    # Extra contribution to delta4
    beta[defNoCD4] <- beta[defNoCD4] + 0.4
    # Keep delta's constant over time
    if (param$NoDelta > defNoCD4) {
      beta[(defNoCD4 + 1):param$NoDelta] <- 0
    }

    for (j in seq_len(jMax + 1) - 1) {
      # Assume all theta's the same (range: 1 to 10^j_max)
      thetaF <- rep((j + 1) * 10 ^ j, param$NoTheta)

      p <- GetParameterVector(beta, thetaF, param)

      res <- FitLLTotal(p, probSurv1996, param, info, data)
      ll <- res$LLTotal

      if (ll < llMin) {
        llMin <- ll
        pParam <- p
        iterResults[[iter]] <- list(
          P = p,
          LLTotal = res$LLTotal,
          ModelResults = res$ModelResults
        )
      }
    }
  }
  message('  Run time: ', format(Sys.time() - startTime))

  # Stop fitting when the change in deviance is smaller than ctol.
  llOld <- 0
  while (
    abs(iterResults[[iter]]$LLTotal - llOld) > ctol &&
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
    iterResults[[iter]] <- res
    llOld <- iterResults[[iter - 1]]$LLTotal
  }

  lastResults <- iterResults[[iter]]

  # Fill beta and thetaF with the best fitting parameters
  beta[seq_len(param$NoDelta)] <- lastResults$P[seq_len(param$NoDelta)]
  thetaF <- lastResults$P[param$NoDelta + seq_len(param$NoTheta)]
  converged <- abs(lastResults$LLTotal - llOld) <= ctol
  statRes <- FitStatistics(lastResults$ModelResults, info, data, param)
  theta <- GetParamTheta(lastResults$P, param, info)
  deltaM <- GetParamDeltaM(lastResults$P, param)

  # ------------------------------------------------------------------------------------------------
  # Implement FitCheckTheta here...
  # ------------------------------------------------------------------------------------------------

  return(list(
    Converged = converged,
    Beta = beta,
    Theta = theta,
    ThetaF = thetaF,
    DeltaM = deltaM,
    Statistics = statRes,
    IterResults = iterResults
  ))
}
