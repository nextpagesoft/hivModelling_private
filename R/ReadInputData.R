#' ReadInputData
#'
#' Description
#'
#' @param context List of parameters. Required.
#'
#' @return
#' Input data as data.table
#'
#' @examples
#' \dontrun{
#' ReadInputData(context)
#' }
#'
#' @export
ReadInputData <- function(context)
{
  inputDataPath <- context$Settings$InputDataPath
  if (!is.null(inputDataPath) && dir.exists(inputDataPath)) {
    fileNames <- list.files(inputDataPath, pattern = '.csv', full.names = TRUE, ignore.case = FALSE)
    inputData <- setNames(
      lapply(fileNames, fread),
      tools::file_path_sans_ext(basename(fileNames))
    )

    inputData <- lapply(inputData, function(dataSet) {
      setnames(dataSet, c('Year', 'Count'))
      dataSet[, ':='(
        Year = as.integer(Year),
        Count = as.numeric(Count)
      )]
    })
    message(sprintf('File %s read\n', fileNames))
  } else {
    inputData <- list()
  }

  data <- data.table(
    Year = 1980L:2017L,
    N_HIV = 0,
    N_HIV_Stage_1 = 0,
    N_HIV_Stage_2 = 0,
    N_HIV_Stage_3 = 0,
    N_HIV_Stage_4 = 0,
    N_HIV_Stage_5 = 0,
    N_AIDS = 0,
    N_Inf = 0,
    N_Dead = 0,
    N_Dead_U = 0,
    N_Emig = 0,
    C_Inf = 0,
    C_Dead = 0,
    C_Dead_U = 0,
    C_Emig = 0
  )

  data[inputData$HIV,       N_HIV         := Count, on = .(Year)]
  data[inputData$HIV_CD4_1, N_HIV_Stage_1 := Count, on = .(Year)]
  data[inputData$HIV_CD4_2, N_HIV_Stage_2 := Count, on = .(Year)]
  data[inputData$HIV_CD4_3, N_HIV_Stage_3 := Count, on = .(Year)]
  data[inputData$HIV_CD4_4, N_HIV_Stage_4 := Count, on = .(Year)]
  data[inputData$HIVAIDS,   N_HIV_Stage_5 := Count, on = .(Year)]
  data[inputData$AIDS,      N_AIDS        := Count, on = .(Year)]
  data[inputData$Dead,      N_Dead        := Count, on = .(Year)]

  if (!is.null(inputData$`Inf`)) {
    data[inputData$`Inf`, N_Inf := Count, on = .(Year)]
  }
  if (!is.null(inputData$Dead_Und)) {
    data[inputData$Dead_Und, N_Dead := Count, on = .(Year)]
  }
  if (!is.null(inputData$Emig)) {
    data[inputData$inputData$Emig, N_Emig := Count, on = .(Year)]
  }

  data[, ':='(
    C_Inf = cumsum(N_Inf),
    C_Dead = cumsum(N_Dead),
    C_Dead_U = cumsum(N_Dead_U),
    C_Emig = cumsum(N_Emig)
  )]

  return(data)
}
