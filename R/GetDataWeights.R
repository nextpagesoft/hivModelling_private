GetDataWeights <- function(
  data
) {
  # CRAN checks workaround
  N_HIV <- NULL
  N_HIV_Stage_5 <- NULL
  N_HIV_Stage_1 <- NULL
  N_HIV_Stage_2 <- NULL
  N_HIV_Stage_3 <- NULL
  N_HIV_Stage_4 <- NULL

  data[, ':='(
    Prob_CD4 = 0,
    Prob_HIVAIDS = 0
  )]

  data[N_HIV > N_HIV_Stage_5, ':='(
    Prob_CD4 = (N_HIV_Stage_1 + N_HIV_Stage_2 + N_HIV_Stage_3 + N_HIV_Stage_4) / (N_HIV - N_HIV_Stage_5),
    Prob_HIVAIDS = N_HIV_Stage_5 / N_HIV
  )]

  return(invisible(data))
}
