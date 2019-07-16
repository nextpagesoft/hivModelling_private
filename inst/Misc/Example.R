library(hivModelling)

# 1. Provide input settings and model parameters ---------------------------------------------------
#
# 1a. Read XML model file
# args <- ReadModelFile(modelFilePath)

# 1b. Specify arguments directly
#
args <- list(
  Run1 = list(
    Settings = list(
      RunInParallel = TRUE,
      ModelsToRun = c('INCIDENCE'),
      InputDataPath = '~/share/HIV test files/Data/test NL'
    ),
    Parameters = list(
      Models = list(
        INCIDENCE = list(
          Country = 'NL'
        )
      )
    )
  ),
  Run2 = list(
    Settings = list(
      RunInParallel = TRUE,
      ModelsToRun = c('INCIDENCE'),
      InputDataPath = '~/share/HIV test files/Data/test NL'
    ),
    Parameters = list(
      Models = list(
        INCIDENCE = list(
          Country = 'NL'
        )
      )
    )
  )
)

# 2. Run -------------------------------------------------------------------------------------------

# 2a. All sequentially
results <- RunModels(args, future::sequential)

# 2b. All in parallel
results <- RunModels(args, future::multiprocess)

# 2c. Single incidence model
results <- do.call(RunIncidenceModel, args[[1]])

# 2d. Single incidence model
results <- RunIncidenceModel(
  settings = args$Run1$Settings,
  parameters = args$Run1$Parameters
)

# 2e. Single incidence model
results <- RunIncidenceModel(
  settings = list(
    RunInParallel = TRUE,
    ModelsToRun = c('INCIDENCE'),
    InputDataPath = '~/share/HIV test files/Data/test NL'
  ),
  parameters = list(
    Models = list(
      INCIDENCE = list(
        Country = 'NL'
      )
    )
  )
)


# 3. Create output artifacts (plots, reports, etc.) ------------------------------------------------
artifacts <- GetOutputArtifacts(results)


# 4. Testing ---------------------------------------------------------------------------------------
context <- GetRunContext(
  settings = list(
    RunInParallel = TRUE,
    ModelsToRun = c('INCIDENCE'),
    InputDataPath = '~/share/HIV test files/Data/test NL'
  ),
  parameters = list(
    Models = list(
      INCIDENCE = list(
        Country = 'NL'
      )
    )
  )
)

context <- GetRunContext(
  settings = list(
    RunInParallel = TRUE,
    ModelsToRun = c('INCIDENCE'),
    InputDataPath = '~/share/HIV test files/Data/test Ard'
  ),
  parameters = list(
    Models = list(
      INCIDENCE = list(
        Country = 'NL',
        FitDistribution = 'POISSON'
      )
    )
  )
)

data <- ReadInputData(context)

# results <- PerformMainFit(context, data)
results <- PerformMainFit(context, data, maxNoFit = 2, verbose = TRUE)

results$Converged
results$ModelOutputs
