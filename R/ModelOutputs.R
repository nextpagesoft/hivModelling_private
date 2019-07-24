ModelOutputs <- function(
  modelResults,
  countResults,
  timeResults,
  info,
  param,
  data
) {
  # CRAN checks
  `.` <- NULL
  Year <- NULL
  N_HIV_D <- NULL
  N_HIV <- NULL
  N_AIDS_M <- NULL
  N_AIDS_D <- NULL
  N_AIDS <- NULL
  Cum_Inf_M <- NULL
  Cum_Inf_D <- NULL
  C_Inf <- NULL
  N_Inf_M <- NULL
  N_Inf_D <- NULL
  N_Inf <- NULL
  t_diag <- NULL
  D_Avg_Time <- NULL
  N_Alive <- NULL
  Cum_Dead_D <- NULL
  Cum_Und_Dead_M <- NULL
  N_Alive_Diag_M <- NULL
  Cum_HIV_M <- NULL
  N_Und <- NULL
  N_Und_1 <- NULL
  N_Und_2 <- NULL
  N_Und_Inf_p <- NULL
  N_Und_Alive_p <- NULL
  N_Und_PrimInf <- NULL
  N_Und_HIVAIDS_M <- NULL
  N_Und_500 <- NULL
  N_Und_CD4_1_M <- NULL
  N_Und_350 <- NULL
  LL_HIV <- NULL
  LL_AIDS <- NULL
  LL_HIVAIDS <- NULL

  VERY_SML <- 1.0e-20
  noCD4 <- param$NoStage - 1

  deltasDT <- as.data.table(t(simplify2array(timeResults$DeltasList)))
  timeToDiagMedianDT <- as.data.table(t(simplify2array(timeResults$TimeToDiagMedian)))

  # Initialize from modelResults
  outputs <- modelResults[, .(
    Run = 0,
    Year
  )]

  # HIV
  outputs[, c('N_HIV_M', 'Cum_HIV_M', 'N_HIV_Obs_M') :=
            modelResults[, c('N_HIV', 'C_HIV', 'N_HIV_S_Obs')]]
  outputs[data, N_HIV_D := N_HIV, on = c('Year')]

  # CD4
  for (i in seq_len(param$NoStage - 1)) {
    mOutputColNames <- sprintf(c('N_CD4_%d_M', 'N_CD4_%d_Obs_M_NoW', 'N_CD4_%d_Obs_M'), i)
    mInputColNames <- sprintf(c('N_HIV_Stage_%d', 'N_HIV_Stage_S_%d', 'N_HIV_Stage_S_Obs_%d'), i)
    outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

    dOutputColNames <- sprintf(c('N_CD4_%d_D'), i)
    dInputColNames <- sprintf(c('N_HIV_Stage_%d'), i)
    outputs[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]
  }

  # HIV/AIDS
  mOutputColNames <- c('N_HIVAIDS_M', 'N_HIVAIDS_Obs_M')
  mInputColNames <- sprintf(c('N_HIV_Stage_%d', 'N_HIV_Stage_S_Obs_%d'), param$NoStage)
  outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  dOutputColNames <- c('N_HIVAIDS_D')
  dInputColNames <- sprintf(c('N_HIV_Stage_%d'), param$NoStage)
  outputs[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  # AIDS
  outputs[, N_AIDS_M := modelResults$N_AIDS]
  outputs[data, N_AIDS_D := N_AIDS, on = c('Year')]

  # Dead
  dOutputColNames <- c('N_Dead_D', 'Cum_Dead_D')
  dInputColNames <- c('N_Dead', 'C_Dead')
  outputs[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  mOutputColNames <- c('N_Diag_Dead_M', 'Cum_Diag_Dead_M')
  mInputColNames <- c('N_Dead_D', 'C_Dead_D')
  outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  dOutputColNames <- c('N_Und_Dead_D', 'Cum_Und_Dead_D')
  dInputColNames <- c('N_Dead_U', 'C_Dead_U')
  outputs[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  mOutputColNames <- c('N_Und_Dead_M', 'Cum_Und_Dead_M')
  mInputColNames <- c('N_Dead_U', 'C_Dead_U')
  outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]

  # Emig
  dOutputColNames <- c('N_Emig_D', 'Cum_Emig_D')
  dInputColNames <- c('N_Emig', 'C_Emig')
  outputs[data, (dOutputColNames) := data[, dInputColNames, with = FALSE], on = c('Year')]

  # Infections
  outputs[, Cum_Inf_M := modelResults$C_Inf]
  outputs[data, Cum_Inf_D := C_Inf, on = c('Year')]

  outputs[, N_Inf_M := modelResults$N_Inf]
  outputs[data, N_Inf_D := N_Inf, on = c('Year')]

  # Delta
  mOutputColNames <- sprintf('delta%d', seq_len(noCD4))
  outputs[, (mOutputColNames) := deltasDT[, seq_len(noCD4), with = FALSE]]

  # Time to diagnosis
  outputs[, t_diag := timeResults$TimeToDiag]

  mOutputColNames <- c('t_diag_p50', 't_diag_p25', 't_diag_p75')
  outputs[, (mOutputColNames) := timeToDiagMedianDT[, c(2, 1, 3), with = FALSE]]

  # Average diagnosis time
  outputs[, D_Avg_Time := countResults$D_Avg_Time]
  mOutputColNames <- sprintf('t_diag_%d', seq_len(ncol(timeResults$TimeToDiagDist$Dist_TimeToDiag)))
  outputs[, (mOutputColNames) := as.data.table(timeResults$TimeToDiagDist$Dist_TimeToDiag)]

  # Number living with HIV
  outputs[, N_Alive := Cum_Inf_M - Cum_Dead_D - Cum_Und_Dead_M]

  # Number diagnosed and living with HIV
  outputs[, N_Alive_Diag_M := Cum_HIV_M - Cum_Dead_D]

  # Number living with undiagnosed HIV
  outputs[, N_Und := Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M]
  outputs[, N_Und_1 := countResults$N_Und_1]
  outputs[, N_Und_2 := countResults$N_Und_2]

  # Proportion living with undiagnosed HIV of all ever infected
  outputs[, N_Und_Inf_p := 100 * (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M) / (Cum_Inf_M + VERY_SML)]

  # Proportion living with undiagnosed HIV of all those still alive
  outputs[, N_Und_Alive_p :=
            100 * (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M) /
            (Cum_Inf_M - Cum_Dead_D - Cum_Und_Dead_M + VERY_SML)]

  outputs[, N_Und_PrimInf := modelResults$PrimInf]

  mOutputColNames <- sprintf('N_Und_CD4_%d_M', seq_len(noCD4))
  mInputColNames <- sprintf('Undiagnosed_%d', seq_len(noCD4))
  outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]
  outputs[, N_Und_HIVAIDS_M := modelResults[[sprintf('Undiagnosed_%d', param$NoStage)]]]

  outputs[, N_Und_500 := N_Und_PrimInf + N_Und_CD4_1_M]
  outputs[, N_Und_350 :=
            N_Und_HIVAIDS_M +
            get(sprintf('N_Und_CD4_%d_M', param$NoStage - 1)) +
            get(sprintf('N_Und_CD4_%d_M', param$NoStage - 2))]

  # Undiagnosed infections by time since infection
  for (i in seq_len(3)) {
    mOutputColName1 <- sprintf('N_Und_T_%d', i)
    outputs[, (mOutputColName1) := countResults$U_Time[i, ]]
    mOutputColName2 <- sprintf('N_Und_T_%d_p', i)
    outputs[, (mOutputColName2) :=
              100 * get(mOutputColName1) / (Cum_Inf_M - Cum_HIV_M - Cum_Und_Dead_M + VERY_SML)]


    mOutputColName3 <- sprintf('N_Und_PrimInf_T_%d', i)
    outputs[, (mOutputColName3) := countResults$PI_Time[i, ]]

    mOutputColNames <- sprintf('N_Und_CD4_%d_T_%d', seq_len(param$NoStage), i)
    outputs[, (mOutputColNames) := as.data.table(countResults$CD4_U_Time[i, , ])]
  }

  outputs[, LL_HIV := modelResults$LL_Pos_Year]
  mOutputColNames <- sprintf('LL_CD4_%d', seq_len(noCD4))
  mInputColNames <- sprintf('LL_PosCD4_Year_%d', seq_len(noCD4))
  outputs[, (mOutputColNames) := modelResults[, mInputColNames, with = FALSE]]
  outputs[, LL_AIDS := modelResults$LL_AIDS_Year]
  outputs[, LL_HIVAIDS := modelResults$LL_AIDSPos_Year]

  return(outputs)
}
