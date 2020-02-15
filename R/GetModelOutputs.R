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
  info  <- model$Info
  statRes      <- FitStatistics(model$ModelResults, info, data, param)
  countResults <- ModelCountResults(model$ModelResults, info, param)
  timeResults  <- ModelTimeResults(model$ModelResults, info, param)
  mainOutputs  <- ModelOutputs(model$ModelResults, countResults, timeResults, info, param, data)

  modelOutputs <- list(
    Statistics = statRes,
    MainOutputs = mainOutputs
  )

  return(modelOutputs)
}
