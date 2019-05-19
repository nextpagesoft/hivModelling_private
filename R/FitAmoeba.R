FitAmoeba <- function(
  ifit,
  ftol,
  nParam,
  pParam,
  probSurv1996,
  param,
  info,
  data
) {
  pFit <- c(rep(0.1, param$NoDelta),
            rep(200, param$NoTheta))

  amoebaP <- matrix(rep(pParam, nParam + 1),
                    nParam + 1,
                    nParam,
                    byrow = TRUE)
  amoebaY <- rep(0, nParam + 1)

  for (i in seq_len(nParam + 1)) {
    if (i > 1) {
      amoebaP[i, i - 1] <- amoebaP[i, i - 1] + pFit[i - 1] * 2
    }

    p <- amoebaP[i, ]

    res <- FitLLTotal(p,
                      probSurv1996,
                      param,
                      info,
                      data)
    amoebaY[i] <- res$LLTotal
  }

  res <- amoeba(p = amoebaP,
                y = amoebaY,
                ndim = nParam,
                ftol = ftol,
                probSurv1996,
                param,
                info,
                data)

  return(res)
}
