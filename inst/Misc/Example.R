library(data.table)
library(hivModelling)

# RUN ----------------------------------------------------------------------------------------------
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

# context <- GetRunContext(
#   settings = list(
#     RunInParallel = TRUE,
#     ModelsToRun = c('INCIDENCE'),
#     InputDataPath = '~/share/HIV test files/Data/test Ard'
#   ),
#   parameters = list(
#     Models = list(
#       INCIDENCE = list(
#         Country = 'NL',
#         FitDistribution = 'POISSON'
#       )
#     )
#   )
# )

data <- ReadInputData(context)

mainResults <- PerformMainFit(context, data)
# mainResults <- PerformMainFit(context, data, maxNoFit = 2, verbose = TRUE)

# Algorithms checked on a single boostrap iteration (only for general comparison, results differ
# from iteration to iteration):
# NLOPT_LN_NELDERMEAD   - 55.5 sec, LLTotal = 239.5948
# NLOPT_LN_BOBYQA       - 34.7 sec, LLTotal = 239.4752
# NLOPT_LN_SBPLX        - very slow, interrupted
# NLOPT_LN_COBYLA       - not converged, LLTotal = 20000000232.6356
# NLOPT_LN_NEWUOA_BOUND - slow, reached maxNoFit, 1.977 mins, LLTotal = 244.279
# NLOPT_LN_PRAXIS       - slow, reached maxNoFit, 6.144 mins, LLTotal = 238.501
# NLOPT_LN_SBPLX        - 2.778879 mins, LLTotal = 235.5132
bsResultsList <- PerformBootstrapFits(
  bsCount = 20,
  context,
  data,
  mainResults,
  algorithm = 'NLOPT_LN_BOBYQA',
  executionPlan = future::multiprocess
)

plots <- CreateOutputPlots(mainResults, bsResultsList)

# Save results in csv file
data.table::fwrite(
  mainResults$MainOutputs,
  '~/share/HIV test files/Results/FUllData/Result_main.csv',
  sep = ','
)

data.table::fwrite(
  rbindlist(lapply(bsResultsList, '[[', 'MainOutputs')),
  '~/share/HIV test files/Results/FUllData/Result_BS.csv',
  sep = ','
)

# RECONCILIATION -----------------------------------------------------------------------------------
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
