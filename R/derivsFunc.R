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

  dlambdad2x <- 0.0

  dydx[1] <- lambda - param$AlphaP * y[1] - param$Mu * y[1]

  iEq <- 1

  # Undiagnosed cases progressing through stages of infection
  j <- iEq + 1
  dydx[j] <- param$FInit[1] * param$AlphaP * y[1] - (param$Qoppa[1] + delta[1] + param$Mu) * y[j]
  for (i in seq_len(param$NoStage - 1) + 1) {
    j <- iEq + i
    dydx[j] <- param$FInit[i] * param$AlphaP * y[1] + param$Qoppa[i - 1] * y[j - 1] - (param$Qoppa[i] + delta[i] + param$Mu) * y[j]
  }

  iEq <- iEq + param$NoStage

  # Diagnosed cases progressing through stages of infection i
  j <- iEq + 1
  dydx[j] <- delta[1] * y[1 + 1] - param$Qoppa[1] * y[j] - param$Mu * y[j]
  # i <- 3
  for (i in seq_len(param$NoStage - 1) + 1) {
    j <- iEq + i
    dydx[j] <- delta[i] * y[1 + i] + param$Qoppa[i - 1] * y[j - 1] - param$Qoppa[i] * y[j] - param$Mu * y[j]
  }

  # After diagnosed infection (param->NoStage compartments) reset base counter
  iEq <- iEq + param$NoStage
  for (i in seq_len(param$NoStage)) {
    j <- iEq + i
    dydx[j] = delta[i] * y[1 + i]
  }

  # Reset base counter
  iEq <- iEq + param$NoStage

  # Cumulative number of AIDS cases
  j <- iEq + 1
  dydx[j] <-
    delta[param$NoStage] * y[1 + param$NoStage] +
    param$Qoppa[param$NoStage - 1] * y[1 + 2 * param$NoStage - 1]

  # Reset base counter
  iEq <- iEq + 1

  # Cumulative number of diagnosed deaths
  j <- iEq + 1
  dydx[j] <- param$Qoppa[param$NoStage] * y[1 + 2 * param$NoStage]
  for (i in seq_len(param$NoStage)) {
    dydx[j] <- dydx[j] + param$Mu * y[1 + param$NoStage + i]
  }

  # Reset base counter
  iEq <- iEq + 1

  # Cumulative number of undiagnosed deaths
  j <- iEq + 1
  dydx[j] <- param$Qoppa[param$NoStage] * y[1 + param$NoStage] + param$Mu * y[1]
  for (i in seq_len(param$NoStage)) {
    dydx[j] <- dydx[j] + param$Mu * y[1 + i]
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
