odeint <- function(
  ystart,
  nVar,
  x1,
  x2,
  eps,
  h1,
  hMin,
  param,
  info,
  minYear,
  maxYear,
  derivsFuncName,
  tmpYear = 0.0
) {
  derivsFunc <- get(derivsFuncName)
  derivsFuncXptr <- GetDerivsFuncXptr(derivsFuncName)

  VERY_LRG <- 1e+10
  nBad <- 0
  nOk <- 0

  MAXSTP <- 10e+5
  TINY <- 1e-30

  yscal <- rep(0, nVar)
  y <- rep(0, nVar)

  x <- x1
  h <- Sign(h1, x2 - x1)

  y <- ystart

  minLambda <- VERY_LRG

  for (nstp in seq_len(MAXSTP)) {
    derivLambda <- GetBSpline(x, param, info, minYear, maxYear)
    dydx <- derivsFunc(x, y, derivLambda, nVar, param, tmpYear)

    yscal <- abs(y) + abs(dydx * h) + TINY

    if ((x + h - x2) * (x + h - x1) > 0) {
      h <- x2 - x
    }

    res <-
      rkqs(x, y, dydx, nVar, h, eps, yscal, param, info, minYear, maxYear, derivsFuncXptr, tmpYear)
    x <- res$X
    y <- res$Y
    rkqsLambda <- res$MinLambda

    minLambda <- min(minLambda,
                     derivLambda,
                     rkqsLambda)

    if (res$hDid == h) {
      nOk <- nOk + 1
    } else {
      nBad <- nBad + 1
    }

    if ((x - x2) * (x2 - x1) >= 0) {
      ystart <- y
      break
    }

    h <- res$hNext
  }

  return(list(
    YStart = ystart,
    X = x,
    NGood = nOk,
    NBad = nBad,
    MinLambda = minLambda
  ))
}
