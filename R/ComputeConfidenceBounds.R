ComputeConfidenceBounds <- function(
  bsIterResults,
  probs = c(0.025, 0.975)
) {
  # CRAN checks
  `.` <- NULL
  Run <- NULL
  Year <- NULL

  inCols <- c(
    'N_HIV_Obs_M', 'N_CD4_1_Obs_M', 'N_CD4_2_Obs_M', 'N_CD4_3_Obs_M', 'N_CD4_4_Obs_M', 'N_AIDS_M',
    'N_HIVAIDS_Obs_M', 'N_Inf_M', 'N_Alive', 'N_Alive_Diag_M', 'N_Und', 'N_Und_Alive_p', 'delta1',
    'delta2', 'delta3', 'delta4', 't_diag', 't_diag_p50', 't_diag_p25', 't_diag_p75', 'D_Avg_Time',
    'N_Und_CD4_3_M', 'N_Und_CD4_4_M'
  )
  outCols <- unlist(lapply(inCols, paste0, c('_LB', '_UB')))

  confBounds <- rbindlist(lapply(bsIterResults, '[[', 'MainOutputs'))
  confBounds <- confBounds[, c('Run', 'Year', inCols), with = FALSE]
  runs <- confBounds[, unique(Run)]

  confBounds <- rbindlist(
    lapply(
      runs,
      function(run) {
        nRows <- confBounds[Run <= run, .N]
        runVec <- rep(run, nRows)
        confBounds[
          Run <= run,
          setNames(
            unlist(
              lapply(
                inCols,
                function(inCol) {
                  as.list(quantile(get(inCol), probs = probs))
                }
              ),
              recursive = FALSE
            ),
            outCols
          ),
          by = .(Run = runVec, Year)
        ]
      }
    )
  )

  return(confBounds)
}
