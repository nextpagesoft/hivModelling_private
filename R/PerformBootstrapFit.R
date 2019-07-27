#' PerformBootstrapFit
#'
#' Perform bootstrap fit
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param mainResults Main results
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
#' PerformBootstrapFit(context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose = TRUE)
#' }
#'
#' @export
PerformBootstrapFit <- function(
  context,
  data,
  mainResults,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  ...
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

  BIT_SML <- 1.0e-6

  info <- mainResults$Info
  param <- mainResults$Param
  probSurv1996 <- GetProvSurv96(param, info)

  dataBS <- data[, .(Year, Prob_CD4, N_Dead, C_Dead, N_Inf, C_Inf, N_Emig, C_Emig)]
  dataBS[mainResults$ModelResults,
         Prob_HIVAIDS := N_HIV_Stage_S_Obs_5 / (BIT_SML + N_HIV_S_Obs),
         on = .(Year)]

  if (info$ModelFitDist == 'POISSON') {

  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    pp <- mainResults$ModelResults[, .(
      Year,
      Prob = param$RDispAIDS / (param$RDispAIDS + BIT_SML + N_AIDS)
    )]
    dataBS[pp, N_AIDS := rnbinom(.N, param$RDispAIDS, Prob), on = .(Year)]

    colNames <- sprintf('N_HIV_Stage_%d', seq_len(param$NoStage))
    # Compute probabilities
    pp <- mainResults$ModelResults[, modifyList(
      .(Year = Year),
      param$RDispRest / (param$RDispRest + BIT_SML + .SD)
    ), .SDcols = colNames]
    # Generate counts
    pp[, (colNames) := lapply(.SD, function(prob) rnbinom(.N, param$RDispRest, prob)),
       .SDcols = colNames]
    dataBS[pp, (colNames) := pp[, colNames, with = FALSE], on = .(Year)]
    dataBS[, N_HIV := rowSums(.SD), .SDcols = colNames]
  } else {
    stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
  }

  colNames <- colNames[-length(colNames)]
  for (colName in colNames) {
    dataBS[Prob_CD4 >  0 & Prob_CD4 <  1, (colName) := rbinom(.N, get(colName), Prob_CD4)]
    dataBS[Prob_CD4 <= 0 | Prob_CD4 >= 1, (colName) := 0]
  }

  GetDataWeights(dataBS)

  res <- EstimateParameters(
    runType = 'BOOTSTRAP', mainResults, probSurv1996, param, info, data = dataBS, maxNoFit, ctol,
    ftol, ...
  )

  p <- res$P
  converged <- res$Converged
  beta <- res$Beta
  thetaF <- res$ThetaF
  param <- res$Param
  info <- res$Info
  iterResults <- res$IterResults
  lastResults <- res$IterResults[[length(res$IterResults)]]

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
