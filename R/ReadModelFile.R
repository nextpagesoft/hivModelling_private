#' ReadModelFile
#'
#' Reads XML model file
#'
#' @param modelFilePath Model file path. Optional. Default = NULL.
#' @param inputDataPath Input data path. Optional. Default = NULL
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
ReadModelFile <- function(
  modelFilePath = NULL,
  inputDataPath = NULL
) {
  model <- NULL

  if (is.null(inputDataPath)) {
    return(NULL)
  }

  if (!is.null(modelFilePath)) {
    # Model file provided directly
    model <- as_list(read_xml(modelFilePath))
  } else if (isTRUE(file.info(inputDataPath)$isdir)) {
    # Mode file included in directory with data files
    modelFilePath <- list.files(
      inputDataPath,
      pattern = '\\.xml$',
      all.files = TRUE,
      full.names = TRUE
    )[1]
    if (!is.na(modelFilePath)) {
      model <- as_list(read_xml(modelFilePath))
    }
  } else if (tolower(tools::file_ext(inputDataPath)) == 'zip') {
    # Mode file included in zip file with data files
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

  version <- as.integer(model$Model$FileVersion[[1]])
  if (version != 2) {
    warning('Version ', version, ' of model files is not supported')
  }

  # Load risk groups
  riskGroups <- NULL
  if (length(model$Model$Meta$RiskGroups) > 0) {
    riskGroups <- list()
    riskGroupNames <- unname(sapply(sapply(model$Model$Meta$RiskGroups, '[[', 'Name'), '[[', 1))
    riskGroups$PopulationSets <- setNames(lapply(model$Model$Meta$RiskGroups, function(riskGroup) {
      populations <- unname(sapply(sapply(riskGroup$RiskCategories, '[[', 'Name'), '[[', 1))
      selected <- as.logical(unname(sapply(sapply(riskGroup$RiskCategories, '[[', 'IsSelected'), '[[', 1)))
      populations[selected]
    }), riskGroupNames)
    riskGroups$Selected <- riskGroupNames[length(riskGroupNames)]
  }

  incModel <- model$Model$IncidenceModel
  model <- list(
    Settings = list(
      RiskGroups = riskGroups
    ),
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
        Country = toupper(incModel$Country[[1]]),
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
