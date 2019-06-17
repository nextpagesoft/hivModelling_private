# Takes one "quality-controlled" Runge-Kutta step
rkqs <- function(
  x,
  y,
  dydx,
  n,
  htry,
  eps,
  yscal,
  param,
  info,
  GetLambda
) {
  VERY_LRG <- 1e+10
  SAFETY <- 0.9
  PSHRNK <- -0.25
  ERRCON <- 1.89e-4
  PGROW <- -0.2

  h <- htry

  minLambda <- VERY_LRG

  while (TRUE) {
    res <- rkck(x, y, dydx, n, h, param, info, GetLambda)
    yout <- res$YOut
    yerr <- res$YErr
    minLambda <- min(minLambda,
                     res$MinLambda)

    errMax <- max(abs(yerr / yscal), 0) / eps

    if (errMax > 1) {
      hTemp <- SAFETY * h * errMax ^ PSHRNK
      h <- ifelse(h >= 0, max(hTemp, 0.1 * h), min(hTemp, 0.1 * h))
      next
    } else {
      hNext <- ifelse(errMax > ERRCON, SAFETY * h * errMax ^ PGROW, 5 * h)
      hDid <- h
      x <- x + hDid
      y <- yout
      break
    }
  }

  result <- list(
    X = x,
    Y = y,
    hDid = hDid,
    hNext = hNext,
    MinLambda = minLambda
  )

  return(result)
}
