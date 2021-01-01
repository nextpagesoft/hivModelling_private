#' ReadModelFile
#'
#' Reads XML model file
#'
#' @param modelFilePath Model file path. Optional. Default = NULL.
#' @param inputDataPath Input data path. Optional. Default = NULL
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
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
  inputDataPath = NULL,
  verbose = TRUE
) {
  model <- NULL

  if (!is.null(modelFilePath)) {
    # Model file provided directly
    model <- as_list(read_xml(modelFilePath))
  } else if (!is.null(inputDataPath)) {
    if (isTRUE(file.info(inputDataPath)$isdir)) {
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
  }

  if (is.null(model)) {
    PrintAlert('No model file found. Parameters will be determined from data', verbose = verbose)
    return(NULL)
  }

  PrintAlert('Model file {.file {modelFilePath}} loaded', type = 'info', verbose = verbose)

  modelVersion <- as.integer(model$Model$FileVersion[[1]])
  if (modelVersion != 2) {
    PrintAlert('Version {.val {modelVersion}} of model file is not supported', type = 'danger')
    return(NULL)
  }

  if (is.null(inputDataPath)) {
    inputDataPath <- model$Model$Meta$InputDataPath[[1]]
    if (!fs::is_absolute_path(inputDataPath)) {
      inputDataPath <- fs::path_norm(fs::path_join(c(dirname(modelFilePath), inputDataPath)))
    }
    if (!dir.exists(inputDataPath)) {
      PrintAlert(
        'Input data path {.path {model$Model$Meta$InputDataPath[[1]]}}
        specified in the model file does not exist.',
        'It will not be incorporated in to the run context.'
      )
      inputDataPath <- NULL
    }
  }

  # Load risk groups
  riskGroups <- NULL
  if (length(model$Model$Meta$RiskGroups) > 0) {
    riskGroups <- list()
    riskGroupNames <- unname(sapply(sapply(model$Model$Meta$RiskGroups, '[[', 'Name'), '[[', 1))
    riskGroups$PopulationSets <- setNames(lapply(model$Model$Meta$RiskGroups, function(riskGroup) {
      populations <- unname(sapply(sapply(riskGroup$RiskCategories, '[[', 'Name'), '[[', 1))
      selected <-
        as.logical(unname(sapply(sapply(riskGroup$RiskCategories, '[[', 'IsSelected'), '[[', 1)))
      populations[selected]
    }), riskGroupNames)
    riskGroups$Selected <- riskGroupNames[length(riskGroupNames)]
  }

  incModel <- model$Model$IncidenceModel
  model <- list(
    Settings = list(
      RiskGroups = riskGroups,
      InputDataPath = inputDataPath
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
