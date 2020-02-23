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
  modelFilePath <- context$Settings$ModelFilePath
  if (!is.null(args$Settings$ModelFilePath)) {
    modelFilePath <- args$Settings$ModelFilePath
  }

  inputDataPath <- context$Settings$ModelFilePath
  if (!is.null(args$Settings$InputDataPath)) {
    inputDataPath <- args$Settings$InputDataPath
  }

  # Override default context with parameters from the model file (average priority)
  modelIncidenceParams <- ReadModelFile(modelFilePath, inputDataPath)
  if (!is.null(modelIncidenceParams)) {
    # Intervals are merged manually due to a problem with merging data.table objects with modifyList
    modelIntervals <- modelIncidenceParams$Parameters$INCIDENCE$Intervals
    modelIncidenceParams$Parameters$INCIDENCE$Intervals <- NULL
    context <- modifyList(context, modelIncidenceParams)
  }

  # InputDataPath could be initialized from the model file. Override here with the one supplied
  # in the arguments.
  if (!is.null(args$Settings$InputDataPath)) {
    context$Settings$InputDataPath <- args$Settings$InputDataPath
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
  allowedYearRanges <- GetAllowedYearRanges(context$Data)

  # Create intervals
  intervals <- GetIntervalsFromData(
    minYear = allowedYearRanges[['All']][[1]],
    maxYear = allowedYearRanges[['All']][[2]],
    numIntervals = 5,
    firstIntervalEndYear = 1984
  )

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
