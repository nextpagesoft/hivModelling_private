#' PerformBootstrapFits
#'
#' Description
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param mainResults List of nain results. Required.
#'
#' @return
#' Results list object
#'
#' @examples
#' \dontrun{
#' PerformBootstrapFits(context, data, mainResults)
#' }
#'
#' @export
PerformBootstrapFits <- function(context, data, mainResults)
{

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

  BIT_SML <- 1.0e-6

  results <- list()

  info <- mainResults$Info
  param <- mainResults$Param

  dataBS <- data[
    Year %in% mainResults$ModelResults$Year,
    .(Year, Prob_CD4, N_Dead, C_Dead, N_Inf, C_Inf, N_Emig, C_Emig)
  ]
  dataBS[mainResults$ModelResults,
         Prob_HIVAIDS := N_HIV_Stage_S_Obs_5 / (BIT_SML + N_HIV_S_Obs),
         on = .(Year)]

  if (info$ModelFitDist == 'POISSON') {

  } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
    pp <- mainResults$ModelResults[, param$RDispAIDS / (param$RDispAIDS + BIT_SML + N_AIDS)]
    dataBS[, N_AIDS := rnbinom(length(pp), param$RDispAIDS, pp)]

    colNames <- sprintf('N_HIV_Stage_%d', seq_len(param$NoStage))
    pp <- mainResults$ModelResults[, param$RDispRest / (param$RDispRest + BIT_SML + .SD),
                                   .SDcols = colNames]
    nHIVStage <- rnbinom(prod(dim(pp)), param$RDispRest, as.matrix(pp))
    dataBS[, (colNames) := as.data.table(matrix(nHIVStage, .N, param$NoStage))]
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

  return(results)
}
