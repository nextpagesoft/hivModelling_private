ModelAnnualNumbers <- function(
  modelResults,
  probSurv1996,
  probCD4
) {
  indices <- match(modelResults[, 'Year'], rownames(probSurv1996))

  C_HIV <- rowSums(modelResults[, c(
    'C_HIV_Stage_1', 'C_HIV_Stage_2', 'C_HIV_Stage_3', 'C_HIV_Stage_4', 'C_HIV_Stage_5'
  )])

  N_HIV_Stage_1 <- diff(c(0, modelResults[, 'C_HIV_Stage_1']))
  N_HIV_Stage_2 <- diff(c(0, modelResults[, 'C_HIV_Stage_2']))
  N_HIV_Stage_3 <- diff(c(0, modelResults[, 'C_HIV_Stage_3']))
  N_HIV_Stage_4 <- diff(c(0, modelResults[, 'C_HIV_Stage_4']))
  N_HIV_Stage_5 <- diff(c(0, modelResults[, 'C_HIV_Stage_5']))

  N_HIV_Stage_S_1 <- N_HIV_Stage_1 * probSurv1996[indices, 1]
  N_HIV_Stage_S_2 <- N_HIV_Stage_2 * probSurv1996[indices, 2]
  N_HIV_Stage_S_3 <- N_HIV_Stage_3 * probSurv1996[indices, 3]
  N_HIV_Stage_S_4 <- N_HIV_Stage_4 * probSurv1996[indices, 4]
  N_HIV_Stage_S_5 <- N_HIV_Stage_5 * probSurv1996[indices, 5]

  N_HIV <- N_HIV_Stage_1 + N_HIV_Stage_2 + N_HIV_Stage_3 + N_HIV_Stage_4 + N_HIV_Stage_5
  N_HIV_S <- N_HIV_Stage_S_1 + N_HIV_Stage_S_2 + N_HIV_Stage_S_3 + N_HIV_Stage_S_4 + N_HIV_Stage_S_5
  N_HIV_S_Obs <- N_HIV_S

  N_HIV_Stage_S_Obs_1 <- N_HIV_Stage_S_1 * probCD4
  N_HIV_Stage_S_Obs_2 <- N_HIV_Stage_S_2 * probCD4
  N_HIV_Stage_S_Obs_3 <- N_HIV_Stage_S_3 * probCD4
  N_HIV_Stage_S_Obs_4 <- N_HIV_Stage_S_4 * probCD4
  N_HIV_Stage_S_Obs_5 <- N_HIV_Stage_S_5
  N_AIDS   <- diff(c(0, modelResults[, 'C_AIDS']))
  N_Inf    <- diff(c(0, modelResults[, 'C_Inf']))
  N_Dead_U <- diff(c(0, modelResults[, 'C_Dead_U']))
  N_Dead_D <- diff(c(0, modelResults[, 'C_Dead_D']))

  annualResults <- cbind(
    C_HIV,
    N_HIV_Stage_1,
    N_HIV_Stage_2,
    N_HIV_Stage_3,
    N_HIV_Stage_4,
    N_HIV_Stage_5,
    N_HIV_Stage_S_1,
    N_HIV_Stage_S_2,
    N_HIV_Stage_S_3,
    N_HIV_Stage_S_4,
    N_HIV_Stage_S_5,
    N_HIV,
    N_HIV_S,
    N_HIV_S_Obs,
    N_HIV_Stage_S_Obs_1,
    N_HIV_Stage_S_Obs_2,
    N_HIV_Stage_S_Obs_3,
    N_HIV_Stage_S_Obs_4,
    N_HIV_Stage_S_Obs_5,
    N_AIDS,
    N_Inf,
    N_Dead_U,
    N_Dead_D
  )

  return(annualResults)
}
