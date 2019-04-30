# Takes one "quality-controlled" Runge-Kutta step
rkqs <- function(
  y,
  dydx,
  n,
  x,
  htry,
  eps,
  yscal,
  param
) {
  SAFETY <- 0.9
  PSHRNK <- -0.25
  ERRCON <- 1.89e-4
  PGROW <- -0.2

  h <- htry

  while (TRUE) {
    res <- rkck(y, dydx, n, x, h, param)

    errMax <- max(abs(res$YErr / yscal), 0) / eps

    if (errMax > 1) {
      hTemp <- SAFETY * h * errMax^PSHRNK
      h <- ifelse(h >= 0, max(hTemp, 0.1 * h), min(hTemp, 0.1 * h))
      xnew <- x + h
      next
    } else {
      hNext <- ifelse(errMax > ERRCON, SAFETY * h * errMax^PGROW, 5 * h)
      hDid <- h
      x <- x + hDid
      y <- res$YOut
      break
    }
  }

  result <- list(
    X = x,
    Y = y,
    hDid = hDid,
    hNext = hNext
  )

  return(result)
}
