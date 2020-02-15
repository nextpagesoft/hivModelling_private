#' PerformBootstrapFit
#'
#' Perform bootstrap fit
#'
#' @param runId Bootstrap run index. Must be greater than 0, which is reserved for the main fit.
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param mainResults Main results
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param verbose Logical indicating to print detailed info during fitting. Optional.
#'   Default = \code{FALSE}
#'
#' @return
#' Results list object
#'
#' @examples
#' \dontrun{
#' PerformBootstrapFit(context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose = FALSE)
#' }
#'
#' @export
PerformBootstrapFit <- function(
  runId,
  context,
  data,
  mainResults,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  verbose = FALSE
) {

  # CRAN checks workaround
  Year <- NULL
  `.` <- NULL
  Prob_CD4 <- NULL
  N_Dead <- NULL
  C_Dead <- NULL
  N_Inf <- NULL
  C_Inf <- NULL
  N_Emig <- NULL
  C_Emig <- NULL
  Prob_HIVAIDS <- NULL
  N_HIV_Stage_S_Obs_5 <- NULL
  N_HIV_S_Obs <- NULL
  N_AIDS <- NULL
  N_HIV <- NULL
  Prob <- NULL
  C_Dead_U <- NULL
  N_Dead_U <- NULL

  BIT_SML <- 1e-6

  info <- mainResults$Info
  param <- mainResults$Param
  probSurv1996 <- GetProvSurv96(param, info)

  # Generate 'data' object
  dataBS <- data[, .(
    Year, Prob_CD4, N_Dead, N_Dead_U, C_Dead, C_Dead_U, N_Inf, C_Inf, N_Emig, C_Emig,
    Prob_HIVAIDS = 0
  )]

  if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    pp <- mainResults$ModelResults[, .(
      Year,
      Prob = param$RDispAIDS / (param$RDispAIDS + BIT_SML + N_AIDS)
    )]
    dataBS[pp, N_AIDS := rnbinom(.N, param$RDispAIDS, Prob), on = .(Year)]

    inColNames <- sprintf('N_HIV_Stage_S_%d', seq_len(param$NoStage))
    outColNames <- sprintf('N_HIV_Stage_%d', seq_len(param$NoStage))
    # Compute probabilities
    pp <- mainResults$ModelResults[, modifyList(
      .(Year = Year),
      param$RDispRest / (param$RDispRest + BIT_SML + .SD)
    ), .SDcols = inColNames]
    # Generate counts
    pp[, (outColNames) := lapply(.SD, function(prob) rnbinom(.N, param$RDispRest, prob)),
       .SDcols = inColNames]
    dataBS[pp, (outColNames) := pp[, outColNames, with = FALSE], on = .(Year)]
    dataBS[, N_HIV := rowSums(.SD), .SDcols = outColNames]
  } else if (info$ModelFitDist == 'POISSON') {
    dataBS[, N_AIDS := rpois(.N, mainResults$MainOutputs$N_AIDS_M)]
    inColNames <- sprintf('N_HIV_Stage_S_%d', seq_len(param$NoStage))
    outColNames <- sprintf('N_HIV_Stage_%d', seq_len(param$NoStage))
    pp <- mainResults$ModelResults[, union('Year', inColNames), with = FALSE]
    pp[, (outColNames) := lapply(.SD, function(col) rpois(.N, col)), .SDcols = inColNames]
    dataBS[pp, (outColNames) := pp[, outColNames, with = FALSE], on = .(Year)]
    dataBS[, N_HIV := rowSums(.SD), .SDcols = outColNames]
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  outColNames <- outColNames[-length(outColNames)]
  for (colName in outColNames) {
    dataBS[Prob_CD4 >  0 & Prob_CD4 <  1, (colName) := rbinom(.N, get(colName), Prob_CD4)]
    dataBS[Prob_CD4 <= 0 | Prob_CD4 >= 1, (colName) := 0]
  }

  GetDataWeights(dataBS)

  # Perform fit on the generated data
  res <- EstimateParameters(
    runType = 'BOOTSTRAP', probSurv1996, param, info, dataBS, mainResults, maxNoFit, ctol, ftol,
    verbose
  )

  p <- res$P
  converged <- res$Converged
  beta <- res$Beta
  thetaF <- res$ThetaF
  param <- res$Param
  info <- res$Info
  iterResults <- res$IterResults

  res <- FitLLTotal(p, probSurv1996, param, info, dataBS)
  modelResults <- res$ModelResults
  statRes <- FitStatistics(modelResults, info, dataBS, param)

  countResults <- ModelCountResults(modelResults, info, param)
  timeResults <- ModelTimeResults(modelResults, info, param)
  mainOutputs <- ModelOutputs(modelResults, countResults, timeResults, info, param, dataBS, runId)

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
