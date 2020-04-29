UpdateThetaParams <- function(
  info,
  param
) {
  # Initialize all thetaP at 1, i.e. no spline weight fixed
  thetaP <- rep(1, info$ModelSplineN)
  if (info$StartIncZero) {
    # Incidence required to start at zero
    thetaP[1] <- 0
  }
  if (info$SplineType == 'B-SPLINE') {
    if (info$MaxIncCorr) {
      thetaP[info$ModelSplineN] <- 0
    }

    thetaP[seq_len(param$NoThetaFix)] <- 0
  }

  param[['ThetaP']] <- thetaP
  param[['NoTheta']] <- sum(thetaP)
  param[['Theta']][thetaP == 0] <- 0

  return(param)
}
