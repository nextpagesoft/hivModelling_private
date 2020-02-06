#' PreprocessInputData
#'
#' Description
#'
#' @param inputData List of input data. Required.
#'
#' @return
#' Input data as data.table
#'
#' @examples
#' \dontrun{
#' PreprocessInputData(inputData)
#' }
#'
#' @export
PreprocessInputData <- function(inputData)
{
  # CRAN checks
  `.` <- NULL
  Year <- NULL
  N_HIV <- NULL
  Count <- NULL
  N_HIV_Stage_1 <- NULL
  N_HIV_Stage_2 <- NULL
  N_HIV_Stage_3 <- NULL
  N_HIV_Stage_4 <- NULL
  N_HIV_Stage_5 <- NULL
  N_AIDS <- NULL
  N_Dead <- NULL
  N_Dead_U <- NULL
  N_Inf <- NULL
  N_Emig <- NULL

  if (is.null(inputData)) {
    return(NULL)
  }

  for (i in seq_along(inputData)) {
    setnames(inputData[[i]], 1, 'Year')
  }

  # Check that populations names are aligned
  CompareColNames <- function(dt, populationNames) {
    identical(colnames(dt)[-1], populationNames)
  }
  populationNames <- colnames(inputData[['HIV_CD4_1']])[-1]
  allColNamesEqual <- all(sapply(inputData, CompareColNames, populationNames = populationNames))

  if (!allColNamesEqual) {
    stop('Input data files have misaligned column names for populations')
  }

  inputData <- setNames(lapply(populationNames, function(populationName) {
    lapply(inputData, function(dataSet) {
      dataSet[, .(Year = as.integer(Year), Count = as.numeric(get(populationName)))]
    })
  }), populationNames)

  yearsList <- lapply(inputData[[1]], '[[', 'Year')
  minYear <- min(sapply(yearsList, min))
  maxYear <- max(sapply(yearsList, max))

  WorkFunc <- function(inputData) {
    data <- data.table(
      Year = minYear:maxYear,
      N_HIV = 0,
      N_HIV_Stage_1 = 0,
      N_HIV_Stage_2 = 0,
      N_HIV_Stage_3 = 0,
      N_HIV_Stage_4 = 0,
      N_HIV_Stage_5 = 0,
      N_AIDS = 0,
      N_Dead = 0,
      N_Dead_U = 0,
      N_Inf = 0,
      N_Emig = 0,
      C_Inf = 0,
      C_Dead = 0,
      C_Dead_U = 0,
      C_Emig = 0
    )

    data[inputData$HIV,       N_HIV         := na.zero(Count), on = .(Year)]
    data[inputData$HIV_CD4_1, N_HIV_Stage_1 := na.zero(Count), on = .(Year)]
    data[inputData$HIV_CD4_2, N_HIV_Stage_2 := na.zero(Count), on = .(Year)]
    data[inputData$HIV_CD4_3, N_HIV_Stage_3 := na.zero(Count), on = .(Year)]
    data[inputData$HIV_CD4_4, N_HIV_Stage_4 := na.zero(Count), on = .(Year)]
    data[inputData$HIVAIDS,   N_HIV_Stage_5 := na.zero(Count), on = .(Year)]
    data[inputData$AIDS,      N_AIDS        := na.zero(Count), on = .(Year)]
    data[inputData$Dead,      N_Dead        := na.zero(Count), on = .(Year)]

    if (!is.null(inputData$Dead_Und)) {
      data[inputData$Dead_Und, N_Dead_U := na.zero(Count), on = .(Year)]
    }
    if (!is.null(inputData$`Inf`)) {
      data[inputData$`Inf`, N_Inf := na.zero(Count), on = .(Year)]
    }
    if (!is.null(inputData$Emig)) {
      data[inputData$inputData$Emig, N_Emig := na.zero(Count), on = .(Year)]
    }

    data[, ':='(
      C_Inf = cumsum(N_Inf),
      C_Dead = cumsum(N_Dead),
      C_Dead_U = cumsum(N_Dead_U),
      C_Emig = cumsum(N_Emig)
    )]

    GetDataWeights(data)
  }

  inputData <- lapply(inputData, WorkFunc)

  return(inputData)
}
