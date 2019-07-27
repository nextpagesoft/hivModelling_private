#' EstimateParameters
#'
#' Estimate parameters
#'
#' @param runType String indicating type of run, either 'MAIN' or 'BOOSTRAP'
#' @param mainResults Main results
#' @param probSurv1996  Test
#' @param param List of parameters. Required.
#' @param info List of parameters. Required.#'
#' @param data Input data as data.table. Required.
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param ... Additional arguments passed to amoeba function. Optional.
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' EstimateParameters(runType, mainResults, probSurv1996, param, info, data, maxNoFit, ctol, ftol)
#' }
#'
#' @export
EstimateParameters <- function(
  runType,
  mainResults = NULL,
  probSurv1996,
  param,
  info,
  data,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  ...
) {
  OptimFunc <- function(p) {
    res <- FitLLTotal(p, probSurv1996, param, info, data)
    return(res$LLTotal)
  }

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

  startTime <- Sys.time()
  message('--- Iteration ', iter, ': Scale')
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
  } else {
    beta <- mainResults$Beta
    thetaF <- mainResults$ThetaF
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
  message('  Run time: ', format(Sys.time() - startTime))

  # Stop fitting when the change in deviance is smaller than ctol.
  llOld <- 0
  while (
    abs(iterResults[[iter]]$LLTotal - llOld) > ctol &&
    iter < maxNoFit
  ) {
    iter <- iter + 1

    if (runType == 'MAIN') {
      message('--- Iteration ', iter, ': Amoeba')
      startTime <- Sys.time()
      res <- FitAmoeba(iter, ftol, nParam, pParam, probSurv1996, param, info, data, ...)
      message('  Run time: ', format(Sys.time() - startTime))
    } else {
      message('--- Iteration ', iter, ': nloptr')
      startTime <- Sys.time()

      # Algorithms that work:
      # NLOPT_LN_NELDERMEAD - accurate, slow
      # NLOPT_LN_BOBYQA - moderately accurate, fast
      # NLOPT_LN_SBPLX - accurate, slow
      optimRes <- nloptr::nloptr(
        pParam,
        OptimFunc,
        lb = c(rep(0, param$NoDelta), rep(-Inf, param$NoTheta)),
        ub = c(rep(1, param$NoDelta), rep(Inf, param$NoTheta)),
        opts = list(
          algorithm = 'NLOPT_GN_ORIG_DIRECT_L',
          xtol_rel = 1.0e-8,
          print_level = 0,
          maxeval = 100000
        )
      )

      # test <- nlm(OptimFunc, pParam)
      # OptimFunc(optimRes$solution)
      # OptimFunc(pParam)

      p <- optimRes$solution
      fitRes <- FitLLTotal(p, probSurv1996, param, info, data)
      res <- modifyList(
        list(P = p),
        fitRes
      )

      message('  Run time: ', format(Sys.time() - startTime))
    }

    pParam <- res$P
    iterResults[[iter]] <- res
    llOld <- iterResults[[iter - 1]]$LLTotal
  }

  lastResults <- iterResults[[iter]]

  converged <- abs(lastResults$LLTotal - llOld) <= ctol
  beta[seq_len(param$NoDelta)] <- lastResults$P[seq_len(param$NoDelta)]
  thetaF <- lastResults$P[param$NoDelta + seq_len(param$NoTheta)]
  param$Theta <- GetParamTheta(lastResults$P, param, info)
  param$DeltaM <- GetParamDeltaM(lastResults$P, param)

  selSmallTheta <- abs(param$Theta) < 1
  param$ThetaP[selSmallTheta] <- 0
  param$Theta[selSmallTheta] <- 0
  param$NoTheta <- sum(param$ThetaP)

  invisible(list(
    Converged = converged,
    P = lastResults$P,
    Beta = beta,
    Theta = param$theta,
    ThetaF = thetaF,
    DeltaM = param$DeltaM,
    Info = info,
    Param = param,
    IterResults = iterResults
  ))
}
