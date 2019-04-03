#' Run incidence model
#'
#' Executes the incidence model
#'
#' @param ... Arguments to be passed to \code{\link{GetRunContext}}.
#'
#' @return
#' list object containing context, data and model objects
#'
#' @examples
#' RunIncidenceModel()
#'
#' @export
RunIncidenceModel <- function(...)
{
  context <- GetRunContext(...)

  data <- ReadInputData(context)

  model <- PerformMainFit(context, data)

  model <- PerformBootstrapFits(context, data, model)

  results <- list(
    Context = context,
    Data = data,
    Model = model
  )

  return(results)
}
