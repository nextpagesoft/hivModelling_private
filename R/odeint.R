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
  derivsFunc,
  tmpYear = 0
) {
  if (info$SplineType == 'B-SPLINE') {
    GetLambda <- GetBSpline_c
  } else if (info$SplineType == 'M-SPLINE') {
    stop('GetLambda for info$SplineType == "M-SPLINE" is not yet implemented')
  } else {
    stop(sprintf('info$SplineType equal "%s" is not supported', info$SplineType))
  }

  VERY_LRG <- 1e+10
  nBad <- 0
  nOk <- 0

  MAXSTP <- 10e+5
  TINY <- 1e-30

  yscal <- rep(0, nVar)
  y <- rep(0, nVar)

  x <- x1
  h <- Sign_c(h1, x2 - x1)

  y <- ystart

  minLambda <- VERY_LRG

  for (nstp in seq_len(MAXSTP)) {
    derivLambda <- GetLambda(x, param, info, minYear, maxYear)
    dydx <- derivsFunc(x, y, derivLambda, nVar, param, tmpYear)

    yscal <- abs(y) + abs(dydx * h) + TINY

    if ((x + h - x2) * (x + h - x1) > 0) {
      h <- x2 - x
    }

    res <- rkqs(
      x, y, dydx, nVar, h, eps, yscal, param, info, GetLambda, minYear, maxYear, derivsFunc,
      tmpYear
    )
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
