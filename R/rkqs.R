# Takes one "quality-controlled" Runge-Kutta step
rkqs <- function(
  y,
  dydx,
  n,
  x,
  htry,
  eps,
  yscal
) {
  SAFETY <- 0.9
  PSHRNK <- -0.25
  ERRCON <- 1.89e-4
  PGROW <- -0.2

  h <- htry

  while (TRUE) {
    res <- rkck(y, dydx, n, x, h)

    errMax <- 0
    for (i in seq_len(n)) {
      errMax <- max(errMax, abs(res$YErr[i] / yscal[i]))
    }

    errMax <- errMax / eps

    if (TRUE) {

    } else {

    }
  }
}
