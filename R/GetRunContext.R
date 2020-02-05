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
  # Check provided arguments
  args <- list(...)
  if (length(names(args)) > 0) {
    names(args) <- CapWords(names(args))
    stopifnot(names(args) %in% c('Settings', 'Parameters', 'Data'))
  }

  # Define default context
  defaultContext <- list(
    Settings   = GetObjectDefinition('Settings'),
    Parameters = GetObjectDefinition('Parameters')
  )

  # Incorporate provided arguments
  context <- modifyList(defaultContext, args)

  # Read model settings file, if exists
  incidenceParams <- ReadModelFile(
    context$Settings$ModelFilePath,
    context$Settings$InputDataPath
  )
  if (!is.null(incidenceParams)) {
    context$Parameters$INCIDENCE$Intervals <- incidenceParams$Parameters$INCIDENCE$Intervals
    context <- modifyList(context, incidenceParams)
  }

  # Read input data
  if (is.null(context$Data)) {
    data <- ReadInputData(context$Settings$InputDataPath)
    context <- modifyList(
      context,
      list(Data = data),
      keep.null = TRUE
    )
  }

  # Preprocess input data
  preprocessedData <- PreprocessInputData(context$Data)

  context <- modifyList(
    context,
    list(PreprocessedData = preprocessedData),
    keep.null = TRUE
  )

  return(context)
}
