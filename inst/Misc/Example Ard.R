library(data.table)
library(hivModelling)

# RUN ----------------------------------------------------------------------------------------------
context <- GetRunContext(
  settings = list(
    InputDataPath = 'D:/VirtualBox_Shared/Ard test/Data/',
    Verbose = TRUE
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
plots$`Time to diagnosis, by year of diagnosis`
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

a <- prettyunits::pretty_dt(ISOdate(2020, 1, 1) - ISOdate(2020, 1, 1))
PrintAlert('{.timestamp {a}}')

items <- c('*' = 1, '*' = 2)
items <- c(1, 2)
setNames(items, rep('*', length(items)))
names(items) <- rep('*', length(items))

cli::cli_bullets(vec)
cli::cli_bullets(c(
  "noindent",
  " " = "indent",
  "*" = "bullet",
  ">" = "arrow",
  "v" = "success",
  "x" = "danger",
  "!" = "warning",
  "i" = "{.timestamp {a}}"
))
cli::cli_li
PrintAlert('{.timestamp 3.4s}')
cli::cli_verbatim("Beginning of the {.emph 1} item")
test <- cli::cli_list_themes()
test$`cli-17096-2`$style

a <- 'AMOEBA'
PrintAlert(
  'Iteration {sprintf("%02d", 1)}: {a}',
  '{.timestamp {prettyunits::pretty_dt(Sys.time() - Sys.time())}}',
  type = 'success'
)
nums <- 1:5 / 7
test <- cli::cli_format(nums, style = list(digits = 2))
cli::cli_text("{.val {nums}}")
cli::cli_text("{.val {test}}")

cli:::cli_format.numeric
sprintf("%10.5G", 1234.234324)
sprintf("%10.6g", 1234.234324)
sprintf("%10.6f", 1234.234324)
sprintf("%15.6f", 1234234.234324)

cli:::cli_format.character
cli:::cli_format.character

sprintf("%7s", 'AMOEBA')

library(cli)
fmt <- ansi_columns(
paste(col_red("foo"), 1:10),
width = 50,
fill = "rows",
max_cols=10,
align = "center",
sep = " "
)
fmt
ansi_nchar(fmt, type = "width")
boxx(fmt, padding = c(0,1,0,1), header = col_green("foobar"))
