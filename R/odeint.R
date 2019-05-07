odeint <- function(
  ystart,
  nVar,
  x1,
  x2,
  eps,
  h1,
  hMin,
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

  y <- ystart

  # nstp <- 1
  for (nstp in seq_len(MAXSTP)) {
  # for (nstp in 1:71) {
    dydx <- derivsFunc(x, y, nVar, param, info)

    yscal <- abs(y) + abs(dydx * h) + TINY

    if ((x + h - x2) * (x + h - x1) > 0.0) {
      h <- x2 - x
    }

    res <- rkqs(y, dydx, n = nVar, x, htry = h, eps, yscal, param, info)
    x <- res$X
    y <- res$Y

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

  return(list(YStart = ystart, NGood = nOk, NBad = nBad))
}
