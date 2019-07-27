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

# mainResults <- PerformMainFit(context, data, maxNoFit = 2, verbose = TRUE)
mainResults <- PerformMainFit(context, data)
bsResults <- PerformBootstrapFit(context, data, mainResults)
bsResultsList <- PerformBootstrapFits(bsCount = 4, context, data, mainResults)

# Reconcile against the Windows version
newVer <- mainResults$MainOutputs
newVer[, Version := 'R']
oldVer <- fread('~/share/HIV test files/Results/FullData/pop_0_Result_main.csv')
oldVer[, ':='(
  Version = 'C',
  Timestamp = NULL
)]
setnames(oldVer, old = c('run', 'year'), new = c('Run', 'Year'))

compareDT <- rbind(newVer, oldVer)
keyCols <- c('Version', 'Run', 'Year')
numCols <- setdiff(colnames(compareDT), keyCols)
compareDT <- compareDT[, lapply(.SD, sum), .SDcols = numCols, by = .(Version)]
compareDT <- melt(compareDT, id.vars = 'Version', variable.name = 'Column', value.name = 'Value')
compareDT <- dcast(compareDT, Column ~ Version, value.var = 'Value')
compareDT[, Difference := R - C]
errors <- compareDT[abs(Difference) > 1e-3]
if (nrow(errors) > 0) {
  message('Reconciliation failed:')
  print(errors)
} else {
  message('Reconciliation successful')
}

# Save results in csv file
data.table::fwrite(
  mainResults$MainOutputs,
  '~/share/HIV test files/Results/FUllData/Result_main.csv',
  sep = ','
)
