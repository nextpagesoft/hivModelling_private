#' ReadInputData
#'
#' Description
#'
#' @param inputDataPath Input data path
#'
#' @return
#' Input data as list
#'
#' @examples
#' \dontrun{
#' ReadInputData(inputDataPath)
#' }
#'
#' @export
ReadInputData <- function(inputDataPath)
{
  inputData <- NULL
  if (!is.null(inputDataPath) && file.exists(inputDataPath)) {
    # Unzip zip file
    if (!isTRUE(file.info(inputDataPath)$isdir)) {
      inDir <- tempfile()
      dir.create(inDir, recursive = TRUE)
      on.exit({
        unlink(inDir, recursive = TRUE, force = TRUE)
      })

      unzip(inputDataPath, exdir = inDir)

      fileNames <- list.files(inDir, recursive = TRUE, full.names = TRUE)
      inputDataPath <- unique(dirname(fileNames))
    }

    # All files should be present in a single folder
    if (length(inputDataPath) == 1L) {
      # Read directory
      fileNames <- list.files(
        inputDataPath,
        pattern = '.csv',
        full.names = TRUE,
        ignore.case = FALSE
      )
      inputData <- setNames(
        lapply(fileNames, fread),
        tools::file_path_sans_ext(basename(fileNames))
      )
      message(sprintf('File %s read\n', fileNames))
    }
  }

  if (is.null(inputData)) {
    return(NULL)
  }

  return(inputData)
}
