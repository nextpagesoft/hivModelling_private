library(data.table)
library(hivModelling)

# RUN ----------------------------------------------------------------------------------------------
context <- GetRunContext(
  settings = list(
    InputDataPath = system.file('TestData/Test_1.zip', package = 'hivModelling'),
    Verbose = TRUE
  ),
  parameters = list(
    INCIDENCE = list(
      Intervals = data.table(
        StartYear = c(1980L, 1984L, 1996L, 2000L, 2005L, 2010L),
        Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
        ChangeInInterval = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
      )
    )
  )
)

data <- GetPopulationData(context)

# Model parameters
beta <- c(0.22418973, 0.19100143, 0.07144893, 0.07608724)
theta <- c(0.0000, 683.7634, 171.1121, 828.2901, 1015.1668, 935.0453, 1058.9732, 1182.9012)

# Fit model
model <- FitModel(beta, theta, context, data, preCompBSpline)

# Get model outputs
modelOutputs <- GetModelOutputs(model, data)

# Create output plots
plots <- CreateOutputPlots(modelOutputs)

years <- seq(
  context$Parameters$INCIDENCE$ModelMinYear,
  context$Parameters$INCIDENCE$ModelMaxYear + 1,
  length.out = 1000
)
incidence <- sapply(
  years,
  GetBSpline,
  theta = model$Param$Theta,
  kOrder = model$Info$SplineOrder,
  modelSplineN = model$Info$ModelSplineN,
  myKnots = model$Info$MyKnots,
  minYear = model$Info$ModelMinYear,
  maxYear = model$Info$ModelMaxYear
)
preCompBSpline <- cbind(
  X = years,
  Y = incidence
)
plot(preCompBSpline)

Test <- function(time = 1980.1, x = years, y = incidence) {
  approx(x, y, time)$y
}
Test(1980.1, x, y)
GetBSplinePreComp(time, preCompBSpline)

sapply(
  c(1980, 1990, 2000, 2010, 2017),
  GetDelta,
  delta4Fac = param$Delta4Fac,
  deltaM = param$DeltaM,
  tc = param$Tc,
  deadStageIdx = param$NoStage
)
p[1]

mainResults <- PerformMainFit(context, data)
mainResults$Param$DeltaP
mainResults$Param$NoDelta
mainResults$P
mainResults$Param$Beta
mainResults$Param$DeltaM

PerformMainFit(context, data, param = mainResults$Param, info = mainResults$Info)

plots <- CreateOutputPlots(mainResults)
# mainResults <- PerformMainFit(context, data, model$Param, model$Info)
bsResultsList <- PerformBootstrapFits(
  context,
  data,
  mainResults,
  bsCount = 20,
  executionPlan = future::multisession
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
