FitStatistics <- function(
  modelResults,
  info,
  data,
  param
) {
  noCD4 <- param$NoStage - 1

  N_LL_AIDSPos <- 0
  N_LL_AIDS <- 0
  N_LL_Pos <- 0
  PearsonX2 <- 0
  LL_Poisson <- 0
  LL_NegBin <- 0
  N_LL_PosCD4 <- rep(0, noCD4)

  N_HIV_Stage_S_Obs_HIVAIDS <- sprintf('N_HIV_Stage_S_Obs_%d', param$NoStage)
  N_HIV_Stage_HIVAIDS <- sprintf('N_HIV_Stage_%d', param$NoStage)

  totModelsHIVAIDS <- modelResults[[N_HIV_Stage_S_Obs_HIVAIDS]]
  totDatasHIVAIDS <- data[[N_HIV_Stage_HIVAIDS]]
  totModelsAIDS <- modelResults[['N_AIDS']]
  totDatasAIDS <- data[['N_AIDS']]
  totModelsHIV <- modelResults[['N_HIV_S_Obs']]
  totDatasHIV <- data[['N_HIV']]
  totModelsCD4 <- modelResults[, sprintf('N_HIV_Stage_S_Obs_%d', seq_len(noCD4)), with = FALSE]
  totDatasCD4 <- data[, sprintf('N_HIV_Stage_%d', seq_len(noCD4)), with = FALSE]

  for (year in seq_len(nrow(modelResults))) {

    # Category HIV/AIDS
    if (
      totModelsHIVAIDS[year] > 0 &&
      modelResults$Year[year] >= info$FitAIDSPosMinYear &&
      modelResults$Year[year] <= info$FitAIDSPosMaxYear
    ) {
      N_LL_AIDSPos <- N_LL_AIDSPos + 1
      if (info$ModelFitDist == 'POISSON') {
        PearsonX2 <-
          PearsonX2 + (totDatasHIVAIDS[year] - totModelsHIVAIDS[year]) ^ 2 / totModelsHIVAIDS[year]
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        PearsonX2 <-
          PearsonX2 +
          (totDatasHIVAIDS[year] - totModelsHIVAIDS[year]) ^ 2 /
          (totModelsHIVAIDS[year] + totModelsHIVAIDS[year] ^ 2 / param$RDispRest)
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }
      LL_Poisson <- LL_Poisson + FitLLPoisson(totModelsHIVAIDS[year], totDatasHIVAIDS[year])
      LL_NegBin <-
        LL_NegBin + FitLLNegBin(totModelsHIVAIDS[year], totDatasHIVAIDS[year], param$RDispRest)
    }

    # AIDS
    if (
      totModelsAIDS[year] > 0 &&
      modelResults$Year[year] >= info$FitAIDSMinYear &&
      modelResults$Year[year] <= info$FitAIDSMaxYear
    ) {
      N_LL_AIDS <- N_LL_AIDS + 1
      if (info$ModelFitDist == 'POISSON') {
        PearsonX2 <-
          PearsonX2 + (totDatasAIDS[year] - totModelsAIDS[year]) ^ 2 / totModelsAIDS[year]
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        PearsonX2 <-
          PearsonX2 +
          (totDatasAIDS[year] - totModelsAIDS[year]) ^ 2 /
          (totModelsAIDS[year] + totModelsAIDS[year] ^ 2 / param$RDispAIDS)
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }
      LL_Poisson <- LL_Poisson + FitLLPoisson(totModelsAIDS[year], totDatasAIDS[year])
      LL_NegBin <- LL_NegBin + FitLLNegBin(totModelsAIDS[year], totDatasAIDS[year], param$RDispAIDS)
    }

    # HIV
    if (
      totModelsHIV[year] > 0 &&
      modelResults$Year[year] >= info$FitPosMinYear &&
      modelResults$Year[year] <= info$FitPosMaxYear &&
      (modelResults$Year[year] < info$FitPosCD4MinYear ||
       modelResults$Year[year] > info$FitPosCD4MaxYear)
    ) {
      N_LL_Pos <- N_LL_Pos + 1
      if (info$ModelFitDist == 'POISSON') {
        PearsonX2 <- PearsonX2 + (totDatasHIV[year] - totModelsHIV[year]) ^ 2 / totModelsHIV[year]
      } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
        PearsonX2 <- PearsonX2 +
          (totDatasHIV[year] - totModelsHIV[year]) ^ 2 /
          (totModelsHIV[year] + totModelsHIV[year] ^ 2 / param$RDispRest)
      } else {
        stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
      }
      LL_Poisson <- LL_Poisson + FitLLPoisson(totModelsHIV[year], totDatasHIV[year])
      LL_NegBin <- LL_NegBin + FitLLNegBin(totModelsHIV[year], totDatasHIV[year], param$RDispRest)
    }

    # HIV by CD4
    for (j in seq_len(noCD4)) {
      if (
        totModelsCD4[[j]][year] > 0 &&
        modelResults$Year[year] >= info$FitPosCD4MinYear &&
        modelResults$Year[year] <= info$FitPosCD4MaxYear
      ) {
        N_LL_PosCD4[j] <- N_LL_PosCD4[j] + 1
        if (info$ModelFitDist == 'POISSON') {
          PearsonX2 <-
            PearsonX2 +
            (totDatasCD4[[j]][year] - totModelsCD4[[j]][year]) ^ 2 / totModelsCD4[[j]][year]
        } else if (info$ModelFitDist == 'NEGATIVE_BINOMIAL') {
          PearsonX2 <- PearsonX2 +
            (totDatasCD4[[j]][year] - totModelsCD4[[j]][year]) ^ 2 /
            (totModelsCD4[[j]][year] + totModelsCD4[[j]][year] ^ 2 / param$RDispRest)
        } else {
          stop(sprintf('info$ModelFitDist equal "%s" is unsupported', info$ModelFitDist))
        }
        LL_Poisson <- LL_Poisson +
          FitLLPoisson(totModelsCD4[[j]][year], totDatasCD4[[j]][year])
        LL_NegBin <- LL_NegBin +
          FitLLNegBin(totModelsCD4[[j]][year], totDatasCD4[[j]][year], param$RDispRest)
      }
    }
  }

  N_LL_Total <- N_LL_AIDSPos + N_LL_AIDS + N_LL_Pos + sum(N_LL_PosCD4)

  return(list(
    N_LL_Total = N_LL_Total,
    N_LL_AIDSPos = N_LL_AIDSPos,
    N_LL_AIDS = N_LL_AIDS,
    N_LL_Pos = N_LL_Pos,
    N_LL_PosCD4 = N_LL_PosCD4,
    PearsonX2 = PearsonX2,
    LL_Poisson = LL_Poisson,
    LL_NegBin = LL_NegBin
  ))
}
