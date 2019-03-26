#' Run incidence model
#'
#' Executes the incidence model
#'
#' @return
#' context list object
#'
#' @examples
#' RunIncidenceModel()
#'
#' @export
RunIncidenceModel <- function()
{
  context <- GetDefaultRunContex()

  context <- SetParameters(context)

  context <- ReadInputData(context)

  context <- PerformMainFit(context)

  context <- PerformBootstrapFits(context)

  return(context)
}
