derivsFunc <- function(
  x,
  y,
  lambda,
  nVar,
  param,
  info
) {
  dydx <- rep(0, nVar)

  delta <- GetDelta(x, param)

  mu <- param$Mu
  qoppa <- param$Qoppa
  fInit <- param$FInit
  alphaP <- param$AlphaP
  noStage <- param$NoStage

  dlambdad2x <- 0.0

  dydx[1] <- lambda - alphaP * y[1] - mu * y[1]

  iEq <- 1

  # Undiagnosed cases progressing through stages of infection
  j <- iEq + 1
  dydx[j] <- fInit[1] * alphaP * y[1] - (qoppa[1] + delta[1] + mu) * y[j]
  for (i in seq_len(noStage - 1) + 1) {
    j <- iEq + i
    dydx[j] <- fInit[i] * alphaP * y[1] + qoppa[i - 1] * y[j - 1] - (qoppa[i] + delta[i] + mu) * y[j]
  }

  iEq <- iEq + noStage

  # Diagnosed cases progressing through stages of infection i
  j <- iEq + 1
  dydx[j] <- delta[1] * y[1 + 1] - qoppa[1] * y[j] - mu * y[j]
  # i <- 3
  for (i in seq_len(noStage - 1) + 1) {
    j <- iEq + i
    dydx[j] <- delta[i] * y[1 + i] + qoppa[i - 1] * y[j - 1] - qoppa[i] * y[j] - mu * y[j]
  }

  # After diagnosed infection (param->NoStage compartments) reset base counter
  iEq <- iEq + noStage
  for (i in seq_len(noStage)) {
    j <- iEq + i
    dydx[j] = delta[i] * y[1 + i]
  }

  # Reset base counter
  iEq <- iEq + noStage

  # Cumulative number of AIDS cases
  j <- iEq + 1
  dydx[j] <-
    delta[noStage] * y[1 + noStage] +
    qoppa[noStage - 1] * y[1 + 2 * noStage - 1]

  # Reset base counter
  iEq <- iEq + 1

  # Cumulative number of diagnosed deaths
  j <- iEq + 1
  dydx[j] <- qoppa[noStage] * y[1 + 2 * noStage]
  for (i in seq_len(noStage)) {
    dydx[j] <- dydx[j] + mu * y[1 + noStage + i]
  }

  # Reset base counter
  iEq <- iEq + 1

  # Cumulative number of undiagnosed deaths
  j <- iEq + 1
  dydx[j] <- qoppa[noStage] * y[1 + noStage] + mu * y[1]
  for (i in seq_len(noStage)) {
    dydx[j] <- dydx[j] + mu * y[1 + i]
  }

  # Reset base counter
  iEq <- iEq + 1

  # Total cumulative incidence
  j <- iEq + 1
  dydx[j] <- lambda

  # After cumulative incidence (1 compartment) reset base counter
  iEq <- iEq + 1

  j <- iEq + 1
  dydx[j] <- dlambdad2x * dlambdad2x

  return(dydx)
}
