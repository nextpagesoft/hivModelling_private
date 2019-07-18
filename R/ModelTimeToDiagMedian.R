ModelTimeToDiagMedian <- function(
  time,
  param,
  info
) {
  hMin <- 0
  h1 <- 0.02
  eps <- 0.0001
  bitSml <- 1e-6

  # Infection year for which diagnosis rates are determined
  tmpYear <- time

  # Number of equations for calculating the distribution of time to diagnosis
  nEq <- 1 + 2 * param$NoStage
  iEq <- 1 + param$NoStage

  ystart <- c(1000, rep(0, nEq - 1))

  min25 <- 100
  min50 <- 100
  min75 <- 100

  t25 <- 0
  t50 <- 0
  t75 <- 0

  j <- 0
  iNowStop <- FALSE
  while (!iNowStop) {
    # Continue calculations until t75 is not updated anymore
    j <- j + 1
    timeA <- (j - 1) * 0.001
    timeB <- timeA + 0.001
    tmpMinYear <- time
    tmpMaxYear <- tmpMinYear + 1

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
                  derivsFunc = derivsTimeFunc,
                  tmpYear = tmpMinYear)

    ystart <- res$YStart

    sumYstart <- sum(tail(ystart, param$NoStage))

    # The time of each quartile is calculated as the midpoint of the time interval in which the
    # proportion diagnosed is closest to the quartile.
    dist <- abs(sumYstart / 1000 - 0.25)
    if (dist < min25) {
      min25 <- dist
      t25 = (timeA + timeB) / 2
    }

    dist <- abs(sumYstart / 1000 - 0.50)
    if (dist < min50) {
      min50 <- dist
      t50 <- (timeA + timeB) / 2
    }

    dist <- abs(sumYstart / 1000 - 0.75)
    if (dist < min75) {
      min75 <- dist
      t75 <- (timeA + timeB) / 2
    } else {
      iNowStop <- TRUE
    }
  }

  return(c(t25, t50, t75))
}
