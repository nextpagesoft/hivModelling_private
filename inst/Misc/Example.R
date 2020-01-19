library(data.table)
library(hivModelling)

# RUN ----------------------------------------------------------------------------------------------

context <- GetRunContext(
  settings = list(
    InputDataPath = '~/share/HIV test files/Data/test NL.zip'
  )
)

data <- ReadInputData(context)

popData <- GetPopulationData(context, data)

mainResults <- PerformMainFit(context, popData)

bsResultsList <- PerformBootstrapFits(context, popData, mainResults, bsCount = 20)

plots <- CreateOutputPlots(mainResults, bsResultsList)

# SAVE OUTPUTS -------------------------------------------------------------------------------------

# Save results in csv file
fwrite(
  mainResults$MainOutputs,
  '~/share/HIV test files/Results/FUllData/Result_main.csv',
  sep = ','
)

fwrite(
  rbindlist(lapply(bsResultsList, '[[', 'MainOutputs')),
  '~/share/HIV test files/Results/FUllData/Result_BS.csv',
  sep = ','
)

# RECONCILE ----------------------------------------------------------------------------------------

# Reconcile against the desktop version
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
