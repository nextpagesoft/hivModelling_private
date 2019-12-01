#' GetPopulationData
#'
#' Get data for combinations of populations
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#'
#' @return
#' Data set as data.table object
#'
#' @examples
#' \dontrun{
#' GetPopulationData(inputData, populationSet = c('pop_0', 'pop_1'))
#' }
#'
#' @export
GetPopulationData <- function(
  context,
  data
) {
  `.` <- NULL
  Year <- NULL

  populationSet <-
    context$Settings$RiskGroups$PopulationSets[[context$Settings$RiskGroups$Selected]]

  if (length(populationSet) == 0) {
    message('No populations have been selected from input data. All populations will be aggregated.')
    populationSet <- names(data)
  }

  if (!all(populationSet %in% names(data))) {
    stop('Populations not present in the input data have been selected')
  }

  dt <- rbindlist(data[populationSet])
  dt <- dt[,
    lapply(.SD, sum, na.rm = TRUE),
    keyby = .(Year),
    .SDcols = setdiff(colnames(dt), c('Year'))
  ]

  GetDataWeights(dt)

  return(dt)
}
