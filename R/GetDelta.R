# Calculate delta, the rate of diagnosis
GetDelta <- function(
  time,
  param
) {
  delta <- rep(0, param$NoStageTot)

  deltaM <- param$DeltaM
  noStage <- param$NoStage
  tc <- param$Tc

  # First determine the time interval
  iTime <- 0
  dist <- 10000
  while (dist >= 0) {
    iTime <- iTime + 1
    dist <- time - tc[iTime]
  }
  iTime <- iTime - 1

  # Position2 <- function() Position(function(x) x > time, param$Tc) - 1

  # i <- 1
  for (i in seq_len(noStage - 1)) {
    delta[i] <- 0
    # j <- 2
    for (j in seq_len(iTime - 1)) {
      delta[i] <- delta[i] + deltaM[i, j]
    }
    delta[i] <-
      delta[i] +
      deltaM[i, iTime] * (time - tc[iTime]) /
      (tc[iTime + 1] - tc[iTime])
  }

  # AIDS stage : diagnosis rate is always constant
  delta[noStage] <- deltaM[noStage, iTime]

  # Check that the diagnosis rate is as expected
  if (delta[noStage] != param$DeltaAIDS) {
    stop('AIDS rate not AIDS rate in Model_GetDelta!!')
  }

  # Add a constant to the diagnosis rate in the penultimate CD4 category
  if (iTime >= 2) {
    delta[noStage - 1] <- delta[noStage - 1] + param$Delta4Fac
  }

  # Diagnosis rate when dead
  delta[param$NoStageTot] <- 0

  return(delta)
}
