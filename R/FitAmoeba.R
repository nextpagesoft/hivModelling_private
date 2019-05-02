FitAmoeba <- function(
  ifit,
  ftol,
  nParam,
  pParam,
  param
) {
  pFit <- c(rep(0.1, param$NoDelta),
            rep(200, param$NoTheta))

  amoebaP <- matrix(0, nParam + 1, nParam)
  amoebaY <- rep(0, nParam + 1)
  amoebaX <- rep(0, nParam)
}
