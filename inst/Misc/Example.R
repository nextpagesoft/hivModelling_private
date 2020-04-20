library(data.table)
library(hivModelling)

# RUN ----------------------------------------------------------------------------------------------

context <- GetRunContext(
  settings = list(
    InputDataPath = '~/share/HIV test files/Data/test NL/'
  )
)

context <- GetRunContext(
  settings = list(
    InputDataPath = '~/share/_HIV_MODELLING/test NL - 2 populations.zip'
  )
)

context <- GetRunContext(
  settings = list(
    InputDataPath = '~/share/HIV Bootstrap/1/'
  )
)

data <- GetPopulationData(context)

# # Model parameters
# beta <- c(0.22418973, 0.19100143, 0.07144893, 0.07608724)
# theta <- c(0.0000, 683.7634, 171.1121, 828.2901, 1015.1668, 935.0453, 1058.9732, 1182.9012)
#
# # Fit model
# model <- FitModel(beta, theta, context, data)
#
# # Get model outputs
# modelOutputs <- GetModelOutputs(model, data)

# Create output plots
# plots <- CreateOutputPlots(modelOutputs)

mainResults <- PerformMainFit(context, data)

PerformMainFit(context, data, mainResults$Param, mainResults$Info)

plots <- CreateOutputPlots(mainResults)
# mainResults <- PerformMainFit(context, data, model$Param, model$Info)
bsResultsList <- PerformBootstrapFits(
  context,
  data,
  mainResults,
  bsCount = 20,
  executionPlan = future::multiprocess
)
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
