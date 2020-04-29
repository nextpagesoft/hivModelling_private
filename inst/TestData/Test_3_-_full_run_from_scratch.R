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
    InputDataPath = GetFilePath('Test_3.zip')
  )
)

data <- GetPopulationData(context)

mainResults <- PerformMainFit(context, data)
# saveRDS(mainResults, GetFilePath('Test_3_-_full_run_results.RDS'))

plots <- CreateOutputPlots(mainResults)

# RECONCILE ----------------------------------------------------------------------------------------
newVer <- mainResults$MainOutputs
newVer[, Version := 'R']
oldVer <- fread(GetFilePath('Test_3_-_Windows_results.csv'))
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
compareDT[, DifferencePerc := Difference / C]
compareDT[, DifferencePercStr := sprintf('%.2f%%', DifferencePerc * 100)]
setorder(compareDT, Column)
errors <- compareDT[abs(DifferencePerc) > 1e-5]
if (nrow(errors) > 0) {
  PrintAlert('Reconciliation failed:', type = 'danger')
  print(errors)
} else {
  PrintAlert('Reconciliation successful', type = 'success')
}
