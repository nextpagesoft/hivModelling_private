FitAmoeba <- function(
  ifit,
  ftol,
  nParam,
  pParam,
  param
) {
  pFit <- c(rep(0.1, param$NoDelta),
            rep(200, param$NoTheta))

  amoebaP <- matrix(rep(pParam, nParam + 1),
                    nParam + 1,
                    nParam,
                    byrow = TRUE)
  # amoebaY <- rep(0, nParam + 1)
  # amoebaX <- rep(0, nParam)

  for (i in seq_len(nParam)) {
    amoebaP[i + 1, i] <- amoebaP[i + 1, i] + pFit[i] * 2

    amoebaX <- amoebaP[i,]

    amoebaY <- FitLLTotal()
  }
}
