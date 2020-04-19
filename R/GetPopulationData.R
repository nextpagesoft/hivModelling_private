#' GetPopulationData
#'
#' Get data for combinations of populations
#'
#' @param context List of parameters. Required.
#' @param populationSet Character vector of names of populations to aggregate in the ouptu data set.
#'   Default = \code{NULL}.
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
  populationSet = NULL
) {
  cli::cli_div(theme = list(span.orange = list(color = 'orange')))
  on.exit({
    cli::cli_end()
  })

  `.` <- NULL
  Year <- NULL

  data <- context$PreprocessedData

  if (is.null(populationSet)) {
    populationSet <-
      context$Settings$RiskGroups$PopulationSets[[context$Settings$RiskGroups$Selected]]
  }

  if (length(populationSet) == 0) {
    cli::cli_alert_info(
      'No populations have been selected from input data. All populations will be aggregated.'
    )
    populationSet <- names(data)
  }
  cli::cli_alert_info(
    sprintf(
      'Data set is being prepared for a combination of the following populations: {.orange %s}',
      paste(populationSet, collapse = ', ')
    )
  )

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
