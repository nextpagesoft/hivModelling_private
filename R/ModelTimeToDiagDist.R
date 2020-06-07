ModelTimeToDiagDist <- function(
  info,
  param
) {
  nEq <- 1 + 2 * param$NoStage

  BIT_SML <- 1e-6
  numYears <- info$ModelNoYears - 1

  Dist_TimeToDiag <- matrix(0, param$DefNoDiagTime, numYears)
  Dist_ProbDiag <- matrix(0, param$DefNoDiagTime, numYears)

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

      ystart <- TimeOdeintReturn(
        ystart, x1 = timeA + BIT_SML, x2 = timeB - BIT_SML, minYear = tmpMinYear,
        maxYear = tmpMaxYear, tmpYear = tmpMinYear
      )

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

    for (j in seq_len(param$DefNoDiagTime)) {
      # Loop through years after infection
      timeA <- info$ModelMinYear + (i - 1) + (j - 1)
      timeB <- timeA + 1

      tmpYear <- min(timeA + 0.5, info$ModelMaxYear - 0.5 - BIT_SML)
      ystart <- TimeOdeintReturn(
        ystart, x1 = timeA + BIT_SML, x2 = timeB - BIT_SML, minYear = tmpMinYear,
        maxYear = tmpMaxYear, tmpYear = tmpYear
      )

      # Cumulative number diagnosed within j years afer infection in year i
      Dist_ProbDiag[j, i] <- sum(tail(ystart, param$NoStage))
    }

    # Get the number diagnosed in each year j as the cumulative number diagnosed
    # within j years minus those diagnosed within j-1 years (note backwards loop)
    indices <- 2:param$DefNoDiagTime
    Dist_ProbDiag[indices, i] <- Dist_ProbDiag[indices, i] - Dist_ProbDiag[indices - 1, i]
  }

  return(list(
    Dist_TimeToDiag = t(Dist_TimeToDiag),
    Dist_ProbDiag = t(Dist_ProbDiag)
  ))
}
