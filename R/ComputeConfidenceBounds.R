ComputeConfidenceBounds <- function(
  bsIterResults,
  probs = c(0.025, 0.975)
) {
  inCols <- c(
    'N_HIV_Obs_M', 'N_CD4_1_Obs_M', 'N_CD4_2_Obs_M', 'N_CD4_3_Obs_M', 'N_CD4_4_Obs_M', 'N_AIDS_M',
    'N_HIVAIDS_Obs_M', 'N_Inf_M', 'N_Alive', 'N_Alive_Diag_M', 'N_Und', 'N_Und_Alive_p', 'delta1',
    'delta2', 'delta3', 'delta4', 't_diag', 't_diag_p50', 't_diag_p25', 't_diag_p75', 'D_Avg_Time',
    'N_Und_CD4_3_M', 'N_Und_CD4_4_M'
  )
  outCols <- setNames(lapply(inCols, paste0, c('_LB', '_UB')), inCols)

  confBounds <- rbindlist(lapply(bsIterResults, '[[', 'MainOutputs'))
  confBounds <- confBounds[, c('Run', 'Year', inCols), with = FALSE]
  runs <- confBounds[, unique(Run)]

  # inCol <- inCols[1]
  # run <- 2
#
#   rbindlist(
#     lapply(
#       runs,
#       function(run) {
#         nRows <- confBounds[Run <= run, .N]
#         confBounds[
#           Run <= run,
#           setNames(as.list(quantile(get(inCol), probs = probs)), outCols[[inCol]]),
#           by = .(Run = rep(run, nRows), Year)
#         ]
#       }
#     )
#   )
#
#
#   confBounds[
#     Run <= run,
#     lapply(
#       inCols,
#       function(inCol) {
#         as.list(quantile(get(inCol), probs = probs))
#       }
#     ),
#     by = .(Run = rep(run, nRows), Year)
#   ]
#
#
#
#
#
#

  for (run in runs) {
    nRows <- confBounds[Run <= run, .N]
    for (inCol in inCols) {
      confBounds[
        Run <= run,
        outCols[[inCol]] := as.list(quantile(get(inCol), probs = probs)),
        by = .(Run = rep(run, nRows), Year)
      ]
    }
  }
  confBounds[, (inCols) := NULL]

  return(confBounds)
}
