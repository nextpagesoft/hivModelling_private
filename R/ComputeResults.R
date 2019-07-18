ComputeResults <- function(
  modelResults,
  modelOutputs,
  modelOutputs2,
  info,
  param,
  data
) {

  VERY_SML <- 1.0e-20
  noCD4 <- param$NoStage - 1

  deltasList <- lapply(modelResults$Year, GetDelta, param)
  deltasDT <- as.data.table(t(simplify2array(deltasList)))
  timeToDiag <- sapply(deltasList, ModelTimeToDiag, param)
  timeToDiagPercList <- lapply(modelResults$Year, ModelTimeToDiagMedian, param, info)
  timeToDiagPercDT <- as.data.table(t(simplify2array(timeToDiagPercList)))

  # Initialize from modelResults
  results <- modelResults[, .(
    Run = 0,
    Year
  )]

  # HIV
  results[, (c('N_HIV_M', 'Cum_HIV_M', 'N_HIV_Obs_M')) := modelResults[, c('N_HIV', 'C_HIV', 'N_HIV_S_Obs'), with = FALSE]]
  results[data, N_HIV_D := N_HIV, on = c('Year')]

  # CD4
  for (i in seq_len(param$NoStage - 1)) {
    mOutputColNames <- sprintf(c('N_CD4_%d_M', 'N_CD4_%d_Obs_M_NoW', 'N_CD4_%d_Obs_M'), i)
    mInputColNames <- sprintf(c('N_HIV_Stage_%d', 'N_HIV_Stage_S_%d', 'N_HIV_Stage_S_Obs_%d'), i)
    results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

    dOutputColNames <- sprintf(c('N_CD4_%d_D'), i)
    dInputColNames <- sprintf(c('N_HIV_Stage_%d'), i)
    results[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]
  }

  # HIV/AIDS
  mOutputColNames <- c('N_HIVAIDS_M', 'N_HIVAIDS_Obs_M')
  mInputColNames <- sprintf(c('N_HIV_Stage_%d', 'N_HIV_Stage_S_Obs_%d'), param$NoStage)
  results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  dOutputColNames <- c('N_HIVAIDS_D')
  dInputColNames <- sprintf(c('N_HIV_Stage_%d'), param$NoStage)
  results[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  # AIDS
  results[, N_AIDS_M := modelResults$N_AIDS]
  results[data, N_AIDS_D := N_AIDS, on = c('Year')]

  # Dead
  dOutputColNames <- c('N_Dead_D', 'Cum_Dead_D')
  dInputColNames <- c('N_Dead', 'C_Dead')
  results[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  mOutputColNames <- c('N_Diag_Dead_M', 'Cum_Diag_Dead_M')
  mInputColNames <- c('N_Dead_D', 'C_Dead_D')
  results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  dOutputColNames <- c('N_Und_Dead_D', 'Cum_Und_Dead_D')
  dInputColNames <- c('N_Dead_U', 'C_Dead_U')
  results[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  mOutputColNames <- c('N_Und_Dead_M', 'Cum_Und_Dead_M')
  mInputColNames <- c('N_Dead_U', 'C_Dead_U')
  results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  # Emig
  dOutputColNames <- c('N_Emig_D', 'Cum_Emig_D')
  dInputColNames <- c('N_Emig', 'C_Emig')
  results[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  # Infections
  results[, Cum_Inf_M := modelResults$C_Inf]
  results[data, Cum_Inf_D := C_Inf, on = c('Year')]

  results[, N_Inf_M := modelResults$N_Inf]
  results[data, N_Inf_D := N_Inf, on = c('Year')]

  # Delta
  mOutputColNames <- sprintf('delta%d', seq_len(noCD4))
  results[, (mOutputColNames) := deltasDT[, seq_len(noCD4), with = FALSE]]

  # Time to diagnosis
  results[, t_diag := timeToDiag]

  mOutputColNames <- c('t_diag_p50', 't_diag_p25', 't_diag_p75')
  results[, (mOutputColNames) := timeToDiagPercDT[, c(2, 1, 3), with = FALSE]]

  # Average diagnosis time
  results[, D_Avg_time := modelOutputs$D_Avg_Time]
  mOutputColNames <- sprintf('t_diag_%d', seq_len(ncol(modelOutputs2$Dist_TimeToDiag)))
  results[, (mOutputColNames) := as.data.table(modelOutputs2$Dist_TimeToDiag)]

  # Number living with HIV
  results[, N_Alive := Cum_Inf_M - Cum_Dead_D - Cum_Und_Dead_M]

  # Number diagnosed and living with HIV
  results[, N_Alive_Diag_M := Cum_HIV_M - Cum_Dead_D]

  # Number living with undiagnosed HIV
  results[, N_Und := Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M]
  results[, N_Und_1 := modelOutputs$N_Und_1]
  results[, N_Und_2 := modelOutputs$N_Und_2]

  # Proportion living with undiagnosed HIV of all ever infected
  results[, N_Und_Inf_p := 100 * (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M) / (Cum_Inf_M + VERY_SML)]

  # Proportion living with undiagnosed HIV of all those still alive
  results[, N_Und_Alive_p :=
            100 * (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M) /
            (Cum_Inf_M - Cum_Dead_D - Cum_Und_Dead_M + VERY_SML)]

  results[, N_Und_PrimInf := modelResults$PrimInf]


  mOutputColNames <- sprintf('N_Und_CD4_%d_M', seq_len(noCD4))
  mInputColNames <- sprintf('Undiagnosed_%d', seq_len(noCD4))
  results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]
  results[, N_Und_HIVAIDS_M := modelResults[[sprintf('Undiagnosed_%d', param$NoStage)]]]

  results[, N_Und_500 := N_Und_PrimInf + N_Und_CD4_1_M]
  results[, N_Und_350 :=
            N_Und_HIVAIDS_M +
            get(sprintf('N_Und_CD4_%d_M', param$NoStage - 1)) +
            get(sprintf('N_Und_CD4_%d_M', param$NoStage - 2))]

  # Undiagnosed infections by time since infection
  for (i in seq_len(3)) {
    mOutputColName1 <- sprintf('N_Und_T_%d', i)
    results[, (mOutputColName1) := modelOutputs$U_Time[i, ]]
    mOutputColName2 <- sprintf('N_Und_T_%d_p', i)
    results[, (mOutputColName2) :=
              100 * get(mOutputColName1) / (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M + VERY_SML)]


    mOutputColName3 <- sprintf('N_Und_PrimInf_T_%d', i)
    results[, (mOutputColName3) := modelOutputs$PI_Time[i, ]]

    mOutputColNames <- sprintf('N_Und_CD4_%d_T_%d', seq_len(param$NoStage), i)
    results[, (mOutputColNames) := as.data.table(modelOutputs$CD4_U_Time[i, , ])]
  }

  results[, LL_HIV := modelResults$LL_Pos_Year]
  mOutputColNames <- sprintf('LL_CD4_%d', seq_len(noCD4))
  mInputColNames <- sprintf('LL_PosCD4_Year_%d', seq_len(noCD4))
  results[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]
  results[, LL_AIDS := modelResults$LL_AIDS_Year]
  results[, LL_HIVAIDS := modelResults$LL_AIDSPos_Year]

  return(results)
}
