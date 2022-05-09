#' GetModelOutputs
#'
#' Get model outputs
#'
#' @param model model
#' @param data data
#'
#' @return list of outputs
#'
#' @examples
#' \dontrun{
#' GetModelOutputs(model, info, param, data)
#' }
#'
#' @export
GetModelOutputs <- function(model, data)
{
  param <- model$Param
  info <- model$Info
  modelResults <- as.data.table(model$ModelResults)
  statRes      <- FitStatistics(modelResults, info, data, param)
  countResults <- ModelCountResults(modelResults, info, param)
  timeResults  <- ModelTimeResults(modelResults$Year, info, param)
  mainOutputs  <- ModelOutputs(modelResults, countResults, timeResults, info, param, data)

  modelOutputs <- list(
    Statistics = statRes,
    MainOutputs = mainOutputs
  )

  return(modelOutputs)
}
