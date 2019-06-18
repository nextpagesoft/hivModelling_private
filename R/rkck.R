# Take a Cash-Karp Runge-Kutta step
rkck <- function(
  x,
  y,
  dydx,
  n,
  h,
  param,
  info,
  GetLambda,
  minYear,
  maxYear
) {
  ak2 <- rep(0, n)
  ak3 <- rep(0, n)
  ak4 <- rep(0, n)
  ak5 <- rep(0, n)
  ak6 <- rep(0, n)
  ytemp <- rep(0, n)
  yerr <- rep(0, n)
  yout <- rep(0, n)

  a2 <- 0.2
  a3 <- 0.3
  a4 <- 0.6
  a5 <- 1.0
  a6 <- 0.875

  b21 <- 0.2
  b31 <- 0.07499999999999999722444
  b32 <- 0.2250000000000000055511
  b41 <- 0.3
  b42 <- -0.9
  b43 <- 1.2
  b51 <- -0.2037037037037036923959
  b52 <- 2.5
  b53 <- -2.592592592592592559697
  b54 <- 1.296296296296296279849
  b61 <- 0.02949580439814814686317
  b62 <- 0.341796875
  b63 <- 0.04159432870370370627366
  b64 <- 0.4003454137731481399243
  b65 <- 0.061767578125
  c1 <- 0.09788359788359787816425
  c3 <- 0.4025764895330112835836
  c4 <- 0.2104377104377104512611
  c6 <- 0.289102202145680386991
  dc5 <- -0.01932198660714285615159

  dc1 <- -0.004293774801587310618878
  dc3 <- 0.01866858609385785294776
  dc4 <- -0.03415502683080806622939
  dc6 <- 0.03910220214568038699099

  ytemp <- y + b21 * h * dydx
  lambda2 <- GetLambda(x + a2 * h, param, info, minYear, maxYear)
  ak2 <-
    derivsFunc_c(x = x + a2 * h, y = ytemp, lambda = lambda2, nVar = n, param)

  ytemp <- y + h * (b31 * dydx + b32 * ak2)
  lambda3 <- GetLambda(x + a3 * h, param, info, minYear, maxYear)
  ak3 <-
    derivsFunc_c(x = x + a3 * h, y = ytemp, lambda = lambda3, nVar = n, param)

  ytemp <- y + h * (b41 * dydx + b42 * ak2 + b43 * ak3)
  lambda4 <- GetLambda(x + a4 * h, param, info, minYear, maxYear)
  ak4 <-
    derivsFunc_c(x = x + a4 * h, y = ytemp, lambda = lambda4, nVar = n, param)

  ytemp <- y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4)
  lambda5 <- GetLambda(x + a5 * h, param, info, minYear, maxYear)
  ak5 <-
    derivsFunc_c(x = x + a5 * h, y = ytemp, lambda = lambda5, nVar = n, param)

  ytemp <- y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5)
  lambda6 <- GetLambda(x + a6 * h, param, info, minYear, maxYear)
  ak6 <-
    derivsFunc_c(x = x + a6 * h, y = ytemp, lambda = lambda6, nVar = n, param)

  yout <- y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6)
  yerr <- h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6)

  minLambda <- min(lambda2, lambda3, lambda4, lambda5, lambda6)

  result <- list(
    YOut = yout,
    YErr = yerr,
    MinLambda = minLambda
  )

  return(result)
}
