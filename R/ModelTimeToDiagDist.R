ModelTimeToDiagDist <- function(
  modelResults,
  info,
  param
) {
  nEq <- 1 + 2 * param$NoStage

  bitSml <- 1e-6
  nbad <- 0
  ngood <- 0
  hMin <- 0.0
  h1 <- 0.02
  eps <- 0.0001
  numYears <- nrow(modelResults)

  Dist_TimeToDiag <- matrix(0, param$DefNoDiagTime, numYears)

  # Expected proportion diagnosed in each year following the year of infection
  # when diagnosis rates remain the same as in the year of infection
  for (i in seq_len(numYears)) {
    tmpMinYear <- info$ModelMinYear + (i - 1)
    tmpMaxYear <- tmpMinYear + 1

    # Start values. Start with 100 infections in primary infection in year i
    ystart <- c(100, rep(0, nEq - 1))

    for (j in seq_len(param$DefNoDiagTime)) {
      # Loop through years after infection
      timeA <- j - 1
      timeB <- timeA + 1

      res <- odeint(ystart,
                    nVar = nEq,
                    x1 = timeA + bitSml,
                    x2 = timeB - bitSml,
                    eps,
                    h1,
                    hMin,
                    param,
                    info,
                    minYear = tmpMinYear,
                    maxYear = tmpMaxYear,
                    derivsFunc = derivsTimeFunc_c)
      ystart <- res$YStart

      # Cumulative number diagnosed within j years afer infection in year i
      Dist_TimeToDiag[j, i] <- sum(tail(ystart, param$NoStage))
    }

    # Get the number diagnosed in each year j as the cumulative number diagnosed
    # within j years minus those diagnosed within j-1 years (note backwards loop)
    indices <- 2:param$DefNoDiagTime
    Dist_TimeToDiag[indices, i] <- Dist_TimeToDiag[indices, i] - Dist_TimeToDiag[indices - 1, i]
  }

  # Real proportion diagnosed in each year following the year of infection
  for (i in seq_len(numYears)) {
    tmpMinYear <- info$ModelMinYear + (i - 1)
    tmpMaxYear <- tmpMinYear + 1

    # Start values.
    ystart <- c(1, rep(0, nEq - 1))


    stop('UNDER DEVELOPMENT')
  }
}
