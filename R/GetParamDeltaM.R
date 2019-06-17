GetParamDeltaM <- function(
  p,
  param
) {
  deltaM <- param$DeltaM

  deltaM[as.logical(param$DeltaP)] <- p[param$DeltaP]
  return(deltaM)
}
