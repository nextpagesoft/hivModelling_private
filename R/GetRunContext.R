#' GetRunContext
#'
#' Description
#'
#' @param ... Parameters to be merged with default parameters and settings.
#'   Optional.
#'
#' @return
#' context list object
#'
#' @examples
#' GetRunContext()
#'
#' @export
GetRunContext <- function(...)
{
  args <- list(...)

  if (length(names(args)) > 0) {
    names(args) <- CapWords(names(args))
    stopifnot(names(args) %in% c('Settings', 'Parameters', 'Data'))
  }

  defaultContext <- list(
    Settings   = GetObjectDefinition('Settings'),
    Parameters = GetObjectDefinition('Parameters')
  )

  context <- modifyList(defaultContext, args)

  incidenceParams <- ReadModelFile(context)
  if (!is.null(incidenceParams)) {
    context$Parameters$INCIDENCE$Intervals <- incidenceParams$Parameters$INCIDENCE$Intervals
    context <- modifyList(context, incidenceParams)
  }

  data <- ReadInputData(context)
  context <- modifyList(
    context,
    list(Data = data),
    keep.null = TRUE
  )

  return(context)
}
