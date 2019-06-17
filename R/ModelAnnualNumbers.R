ModelAnnualNumbers <- function(
  modelResults,
  probSurv1996,
  data
) {

  # CRAN checks
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


  # Implement computation
  data <- list(
    Prob_CD4 = c(
      0.00000000000000000,
      0.00000000000000000,
      0.00000000000000000,
      0.00000000000000000,
      0.041095890410958902,
      0.16766467065868262,
      0.10071942446043165,
      0.095693779904306220,
      0.10362694300518134,
      0.18025751072961374,
      0.21724137931034482,
      0.32013201320132012,
      0.26724137931034481,
      0.32920792079207922,
      0.39808153477218228,
      0.47285067873303166,
      0.61493123772102165,
      0.65109034267912769,
      0.65454545454545454,
      0.70909090909090911,
      0.69825072886297379,
      0.74037267080745339,
      0.79028436018957349,
      0.77968036529680362,
      0.83614697120158887,
      0.81212723658051689,
      0.80482897384305840,
      0.86968576709796674,
      0.84246575342465757,
      0.84871550903901050,
      0.81879194630872487,
      0.86692381870781099,
      0.89529914529914534,
      0.88126361655773422,
      0.91878172588832485,
      0.92537313432835822,
      0.92667706708268316
    )
  )

  modelResults[, ':='(
    C_HIV =
      C_HIV_Stage_1 + C_HIV_Stage_2 + C_HIV_Stage_3 + C_HIV_Stage_4 +
      C_HIV_Stage_5,
    N_HIV_Stage_1 =  diff(c(0, C_HIV_Stage_1)),
    N_HIV_Stage_2 =  diff(c(0, C_HIV_Stage_2)),
    N_HIV_Stage_3 =  diff(c(0, C_HIV_Stage_3)),
    N_HIV_Stage_4 =  diff(c(0, C_HIV_Stage_4)),
    N_HIV_Stage_5 =  diff(c(0, C_HIV_Stage_5))
  )]

  modelResults[, ':='(
    N_HIV_Stage_S_1 = N_HIV_Stage_1 * probSurv1996[seq_len(.N), 1],
    N_HIV_Stage_S_2 = N_HIV_Stage_2 * probSurv1996[seq_len(.N), 2],
    N_HIV_Stage_S_3 = N_HIV_Stage_3 * probSurv1996[seq_len(.N), 3],
    N_HIV_Stage_S_4 = N_HIV_Stage_4 * probSurv1996[seq_len(.N), 4],
    N_HIV_Stage_S_5 = N_HIV_Stage_5 * probSurv1996[seq_len(.N), 5],
    N_HIV =
      N_HIV_Stage_1 + N_HIV_Stage_2 + N_HIV_Stage_3 + N_HIV_Stage_4 +
      N_HIV_Stage_5
  )]

  modelResults[, ':='(
    N_HIV =
      N_HIV_Stage_1 + N_HIV_Stage_2 + N_HIV_Stage_3 + N_HIV_Stage_4 +
      N_HIV_Stage_5,
    N_HIV_S =
      N_HIV_Stage_S_1 + N_HIV_Stage_S_2 + N_HIV_Stage_S_3 + N_HIV_Stage_S_4 +
      N_HIV_Stage_S_5
  )]

  modelResults[, ':='(
    N_HIV_S_Obs = N_HIV_S
  )]

  modelResults[, ':='(
    N_HIV_Stage_S_Obs_1 = N_HIV_Stage_S_1 * data$Prob_CD4,
    N_HIV_Stage_S_Obs_2 = N_HIV_Stage_S_2 * data$Prob_CD4,
    N_HIV_Stage_S_Obs_3 = N_HIV_Stage_S_3 * data$Prob_CD4,
    N_HIV_Stage_S_Obs_4 = N_HIV_Stage_S_4 * data$Prob_CD4,
    N_HIV_Stage_S_Obs_5 = N_HIV_Stage_S_5,
    N_AIDS = diff(c(0, C_AIDS)),
    N_Inf = diff(c(0, C_Inf)),
    N_Dead_U = diff(c(0, C_Dead_U)),
    N_Dead_D = diff(c(0, C_Dead_D))
  )]

  return(invisible(modelResults))
}
