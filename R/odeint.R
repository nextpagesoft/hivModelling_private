odeint <- function(
  ystart,
  nVar,
  x1,
  x2,
  eps,
  h1,
  hMin,
  derivsFunc,
  param,
  info
) {
  nBad <- 0
  nOk <- 0

  MAXSTP <- 10e+5
  TINY <- 1e-30

  yscal <- rep(0, nVar)
  y <- rep(0, nVar)

  x <- x1
  h <- sign(h1, x2 - x1)

  hDid <- 0
  hNext <- 0

  y <- ystart

  # nstp <- 1
  for (nstp in seq_len(MAXSTP)) {
    dydx <- derivsFunc(x, y, nVar)

    for (i in seq_len(nVar)) {
      yscal[i] <- abs(y[i]) + abs(dydx[i] * h) + TINY
    }


    if ((x + h - x2) * (x + h - x1) > 0.0) {
      h <- x2 - x
    }

    rkqs(y, dydx, n = nVar, x, htry = h, eps, yscal)

  }

  return(list(NGood = nGood, NBad = nBad))
}
