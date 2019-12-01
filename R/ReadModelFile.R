#' ReadModelFile
#'
#' Reads XML model file
#'
#' @param context List of parameters. Required.
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' ReadModelFile(context)
#' }
#'
#' @export
ReadModelFile <- function(context)
{
  model <- NULL

  inputDataPath <- context$Settings$InputDataPath
  modelFilePath <- context$Settings$ModelFilePath
  if (!is.null(modelFilePath)) {
    model <- as_list(read_xml(modelFilePath))
  } else if (tolower(tools::file_ext(inputDataPath)) == 'zip') {
    fileList <- unzip(inputDataPath, list = TRUE)
    fileExts <- sapply(fileList$Name, tools::file_ext)
    modelFilePath <- names(fileExts)[tolower(fileExts) == 'xml'][1]
    if (!is.na(modelFilePath)) {
      modelConn <- unz(inputDataPath, modelFilePath)
      model <- as_list(read_xml(modelConn))
    }
  }
  if (is.null(model)) {
    return(NULL)
  }

  message('Model file "', modelFilePath, '" loaded')

  version <- model$Model$FileVersion[[1]]
  if (version != 2) {
    warning('Version ', version, ' of model files is not supported')
  }

  incModel <- model$Model$IncidenceModel

  model <- list(
    Parameters = list(
      INCIDENCE = list(
        ModelMinYear = as.integer(incModel$MinYear[[1]]),
        ModelMaxYear = as.integer(incModel$MaxYear[[1]]),
        FitPosMinYear = as.integer(incModel$MinFitPos[[1]]),
        FitPosMaxYear = as.integer(incModel$MaxFitPos[[1]]),
        FitPosCD4MinYear = as.integer(incModel$MinFitCD4[[1]]),
        FitPosCD4MaxYear = as.integer(incModel$MaxFitCD4[[1]]),
        FitAIDSMinYear = as.integer(incModel$MinFitAIDS[[1]]),
        FitAIDSMaxYear = as.integer(incModel$MaxFitAIDS[[1]]),
        FitAIDSPosMinYear = as.integer(incModel$MinFitHIVAIDS[[1]]),
        FitAIDSPosMaxYear = as.integer(incModel$MaxFitHIVAIDS[[1]]),
        Intervals = rbindlist(lapply(incModel$DiagnosisRates, function(interval) {
          data.table(
            StartYear        = as.integer(interval$StartYear[[1]]),
            Jump             = as.logical(interval$Jump[[1]]),
            ChangeInInterval = as.logical(interval$ChangingInInterval[[1]]),
            DiffByCD4        = as.logical(interval$DifferentByCD4[[1]])
          )
        })),
        Country = incModel$Country[[1]],
        ModelNoKnots = as.integer(incModel$KnotsCount[[1]]),
        StartIncZero = as.logical(incModel$StartIncZero[[1]]),
        FitDistribution = ifelse(
          toupper(incModel$DistributionFit[[1]]) == 'NEGATIVE BINOMIAL',
          'NEGATIVE_BINOMIAL',
          'POISSON'
        ),
        RDisp = as.numeric(incModel$RDisp[[1]]),
        Delta4Fac = as.numeric(incModel$Delta4Fac[[1]]),
        MaxIncCorr = as.logical(incModel$MaxIncCorr[[1]]),
        SplineType = ifelse(
          toupper(incModel$SplineType[[1]]) == 'B-SPLINES',
          'B-SPLINE',
          'M-SPLINE'
        ),
        FullData = as.logical(incModel$FullData[[1]])
      )
    )
  )

  return(model)
}
