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

fullRunResults <- readRDS(GetFilePath('Test_1_-_full_run_results.RDS'))

mainResults <- PerformMainFit(context, data, fullRunResults$Param, fullRunResults$Info)

plots <- CreateOutputPlots(mainResults)

# RECONCILE ----------------------------------------------------------------------------------------
newVer <- mainResults$MainOutputs
newVer[, Version := 'R']
oldVer <- fread(GetFilePath('Test_1_-_Windows_results.csv'))
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
  PrintAlert('Reconciliation failed:', type = 'danger')
  print(errors)
} else {
  PrintAlert('Reconciliation successful', type = 'success')
}
