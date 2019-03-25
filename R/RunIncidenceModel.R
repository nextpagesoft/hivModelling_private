#' Run incidence model
#'
#' Executes the incidence model
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' RunIncidenceModel()
#'
#' @export
RunIncidenceModel <- function()
{
  context <- SetRunContex()

  InitializeDefaultParameters()

  SetParameters()

  ReadInputData(context)

  PerformMainFit(context)

  PerformBootstrapFits(context)

  invisible(NULL)
}
