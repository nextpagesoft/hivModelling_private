library(data.table)
library(hivModelling)

GetFilePath <- function(fileName) {
  path <- system.file('TestData', package = 'hivModelling')
  filePath <- file.path(path, fileName)
  return(filePath)
}

# RUN ----------------------------------------------------------------------------------------------
context <- GetRunContext(
  settings = list(
    InputDataPath = GetFilePath('Test_1.zip')
  )
)

data <- GetPopulationData(context)

mainResults <- readRDS(GetFilePath('Test_1_-_full_run_results.RDS'))

boostrapResults <- PerformBootstrapFits(
  context,
  data,
  mainResults,
  bsCount = 2,
  maxNoFit = 50,
  executionPlan = future::multisession
)

plots <- CreateOutputPlots(mainResults, boostrapResults)
