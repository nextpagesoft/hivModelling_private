GetParamTheta <- function(
  p,
  param,
  info
) {
  k <- param$NoDelta
  theta <- param$Theta

  for (i in seq_len(info$ModelSplineN)) {
    if (param$ThetaP[i] != 0) {
      k <- k + 1
      theta[i] <- p[k]
    }
  }

  if (info$SplineType == 'B-SPLINE') {
    if (info$MaxIncCorr) {
      theta[info$ModelSplineN] <- 2 * theta[info$ModelSplineN - 1] - theta[info$ModelSplineN - 2]
    }
    # B-splines: keep the first NoThetaFix spline weights zero
    theta[seq_len(param$NoThetaFix)] <- 0
  } else {
    stop('GetParamTheta: info$SplineType different than "B-SPLINE" is not yet implemented')
  }

  return(theta)
}
