# Calculate delta, the rate of diagnosis
GetDelta <- function(
  time,
  param
) {
  delta <- rep(0, param$NoStageTot)

  # First determine the time interval
  iTime <- 0
  dist <- 10000
  while (dist >= 0) {
    iTime <- iTime + 1
    dist <- time - param$Tc[iTime]
  }
  iTime <- iTime - 1

  # i <- 1
  for (i in seq_len(param$NoStage - 1)) {
    delta[i] <- 0
    # j <- 2
    for (j in seq_len(iTime - 1)) {
      delta[i] <- delta[i] + param$DeltaM[i, j]
    }
    delta[i] <-
      delta[i] +
      param$DeltaM[i, iTime] * (time - param$Tc[iTime]) /
      (param$Tc[iTime + 1] - param$Tc[iTime])
  }

  # AIDS stage : diagnosis rate is always constant
  delta[param$NoStage] <- param$DeltaM[param$NoStage, iTime]

  # Check that the diagnosis rate is as expected
  if (delta[param$NoStage] != param$DeltaAIDS) {
    stop('AIDS rate not AIDS rate in Model_GetDelta!!')
  }

  # Add a constant to the diagnosis rate in the penultimate CD4 category
  if (iTime >= 2) {
    delta[param$NoStage - 1] <- delta[param$NoStage - 1] + param$Delta4Fac
  }

  # Diagnosis rate when dead
  delta[param$NoStageTot] <- 0

  return(delta)
}
