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
  # Prepare provided arguments
  args <- list(...)
  if (length(names(args)) > 0) {
    names(args) <- CapWords(names(args))
    stopifnot(names(args) %in% c('Settings', 'Parameters', 'Data'))
  }

  # Define default context (lowest priority)
  context <- list(
    Settings   = GetObjectDefinition('Settings'),
    Parameters = GetObjectDefinition('Parameters'),
    Data = NULL,
    PreprocessedData = NULL
  )

  # Extract paths from arguments
  if (!is.null(args$Settings$ModelFilePath)) {
    context$Settings$ModelFilePath <- args$Settings$ModelFilePath
  }
  if (!is.null(args$Settings$InputDataPath)) {
    context$Settings$InputDataPath <- args$Settings$InputDataPath
  }

  # Override default context with parameters from the model file (average priority)
  incidenceParams <- ReadModelFile(context$Settings$ModelFilePath, context$Settings$InputDataPath)
  if (!is.null(incidenceParams)) {
    context$Parameters$INCIDENCE$Intervals <- incidenceParams$Parameters$INCIDENCE$Intervals
    context <- modifyList(context, incidenceParams)
  }

  # Override any settings with those provided directly to this function (highest priority)
  if (!is.null(args$Parameters$INCIDENCE$Intervals)) {
    context$Parameters$INCIDENCE$Intervals <- args$Parameters$INCIDENCE$Intervals
  }
  context <- modifyList(context, args)

  # Read input data
  if (is.null(context$Data)) {
    data <- ReadInputData(context$Settings$InputDataPath)
    context <- modifyList(
      context,
      list(Data = data),
      keep.null = TRUE
    )
  }

  # Determine settings from data
  ValidateData(context$Data)

  # Preprocess input data
  preprocessedData <- PreprocessInputData(
    context$Data,
    minYear = context$Parameters$INCIDENCE$ModelMinYear,
    maxYear = context$Parameters$INCIDENCE$ModelMaxYear
  )

  context <- modifyList(
    context,
    list(PreprocessedData = preprocessedData),
    keep.null = TRUE
  )

  return(context)
}
