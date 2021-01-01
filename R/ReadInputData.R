#' ReadInputData
#'
#' Description
#'
#' @param inputDataPath Input data path
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
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
ReadInputData <- function(
  inputDataPath,
  verbose = TRUE
) {
  inputData <- NULL
  if (is.null(inputDataPath)) {
    return(NULL)
  }

  pathInfo <- file.info(inputDataPath)

  if (!is.na(pathInfo$mode)) {
    # Unzip zip file
    if (!isTRUE(pathInfo$isdir)) {
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
        pattern = '(HIV|HIVAIDS|AIDS|Dead|HIV_CD4_[1-4])\\.csv$',
        full.names = TRUE,
        ignore.case = FALSE
      )
      inputData <- setNames(
        lapply(fileNames, function(fileName) {
          dt <- fread(fileName)
          setnames(dt, 1, 'Year')
          PrintAlert('Data file {.file {fileName}} loaded', verbose = verbose)
          return(dt)
        }),
        tools::file_path_sans_ext(basename(fileNames))
      )
    }
  }

  if (is.null(inputData)) {
    return(NULL)
  }

  return(inputData)
}
