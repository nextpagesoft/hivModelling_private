ModelAnnualNumbers <- function(
  modelResults,
  probSurv1996,
  probCD4
) {

  # CRAN checks false positives workarounds
  C_HIV_Stage_1 <- NULL
  C_HIV_Stage_2 <- NULL
  C_HIV_Stage_3 <- NULL
  C_HIV_Stage_4 <- NULL
  C_HIV_Stage_5 <- NULL
  N_HIV_Stage_1 <- NULL
  N_HIV_Stage_2 <- NULL
  N_HIV_Stage_3 <- NULL
  N_HIV_Stage_4 <- NULL
  N_HIV_Stage_5 <- NULL
  N_HIV_Stage_S_1 <- NULL
  N_HIV_Stage_S_2 <- NULL
  N_HIV_Stage_S_3 <- NULL
  N_HIV_Stage_S_4 <- NULL
  N_HIV_Stage_S_5 <- NULL
  N_HIV_S <- NULL
  C_AIDS <- NULL
  C_Inf <- NULL
  C_Dead_U <- NULL
  C_Dead_D <- NULL

  modelResults[, ':='(
    C_HIV = C_HIV_Stage_1 + C_HIV_Stage_2 + C_HIV_Stage_3 + C_HIV_Stage_4 + C_HIV_Stage_5,
    N_HIV_Stage_1 = diff(c(0, C_HIV_Stage_1)),
    N_HIV_Stage_2 = diff(c(0, C_HIV_Stage_2)),
    N_HIV_Stage_3 = diff(c(0, C_HIV_Stage_3)),
    N_HIV_Stage_4 = diff(c(0, C_HIV_Stage_4)),
    N_HIV_Stage_5 = diff(c(0, C_HIV_Stage_5))
  )]

  modelResults[, ':='(
    N_HIV_Stage_S_1 = N_HIV_Stage_1 * probSurv1996[seq_len(.N), 1],
    N_HIV_Stage_S_2 = N_HIV_Stage_2 * probSurv1996[seq_len(.N), 2],
    N_HIV_Stage_S_3 = N_HIV_Stage_3 * probSurv1996[seq_len(.N), 3],
    N_HIV_Stage_S_4 = N_HIV_Stage_4 * probSurv1996[seq_len(.N), 4],
    N_HIV_Stage_S_5 = N_HIV_Stage_5 * probSurv1996[seq_len(.N), 5],
    N_HIV = N_HIV_Stage_1 + N_HIV_Stage_2 + N_HIV_Stage_3 + N_HIV_Stage_4 + N_HIV_Stage_5
  )]

  modelResults[, ':='(
    N_HIV_S =
      N_HIV_Stage_S_1 + N_HIV_Stage_S_2 + N_HIV_Stage_S_3 + N_HIV_Stage_S_4 + N_HIV_Stage_S_5
  )]

  modelResults[, ':='(
    N_HIV_S_Obs = N_HIV_S
  )]

  modelResults[, ':='(
    N_HIV_Stage_S_Obs_1 = N_HIV_Stage_S_1 * probCD4,
    N_HIV_Stage_S_Obs_2 = N_HIV_Stage_S_2 * probCD4,
    N_HIV_Stage_S_Obs_3 = N_HIV_Stage_S_3 * probCD4,
    N_HIV_Stage_S_Obs_4 = N_HIV_Stage_S_4 * probCD4,
    N_HIV_Stage_S_Obs_5 = N_HIV_Stage_S_5,
    N_AIDS   = diff(c(0, C_AIDS)),
    N_Inf    = diff(c(0, C_Inf)),
    N_Dead_U = diff(c(0, C_Dead_U)),
    N_Dead_D = diff(c(0, C_Dead_D))
  )]

  return(invisible(modelResults))
}
