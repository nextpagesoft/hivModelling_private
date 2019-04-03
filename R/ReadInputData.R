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
    inputData <- lapply(fileNames, fread)
    names(inputData) <- tools::file_path_sans_ext(basename(fileNames))
    message(sprintf('File %s read\n', fileNames))
  } else {
    inputData <- list()
  }

  return(inputData)
}
