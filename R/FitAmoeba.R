FitAmoeba <- function(
  ifit,
  ftol,
  nParam,
  pParam,
  param,
  deltaP,
  deltaM,
  theta,
  thetaP,
  noThetaFix,
  noDelta,
  modelSplineN,
  modelNoYears,
  modelYears,
  splineType,
  maxIncCorr,
  noEq,
  noStage,
  probSurv1996,
  model,
  info,
  data,
  extraResults
) {
  pFit <- c(rep(0.1, param$NoDelta),
            rep(200, param$NoTheta))

  amoebaP <- matrix(rep(pParam, nParam + 1),
                    nParam + 1,
                    nParam,
                    byrow = TRUE)
  amoebaY <- rep(0, nParam + 1)
  # amoebaX <- rep(0, nParam)

  # i <- 1
  for (i in seq_len(nParam + 1)) {
    if (i > 1) {
      amoebaP[i, i - 1] <- amoebaP[i, i - 1] + pFit[i - 1] * 2
    }

    amoebaX <- amoebaP[i, ]
    amoebaY[i] <- FitLLTotal(amoebaX,
                             deltaP,
                             deltaM,
                             theta,
                             thetaP,
                             noThetaFix,
                             noDelta,
                             modelSplineN,
                             modelNoYears,
                             modelYears,
                             splineType,
                             maxIncCorr,
                             noEq,
                             noStage,
                             probSurv1996,
                             model,
                             param,
                             info,
                             data,
                             extraResults)
  }

  pParam <- amoebaP[1, ]

  funkArgs <- list(
    deltaP = deltaP,
    deltaM = deltaM,
    theta = theta,
    thetaP = thetaP,
    noThetaFix = noThetaFix,
    noDelta = noDelta,
    modelSplineN = modelSplineN,
    modelNoYears = modelNoYears,
    modelYears = modelYears,
    splineType = splineType,
    maxIncCorr = maxIncCorr,
    noEq = noEq,
    noStage = noStage,
    probSurv1996 = probSurv1996,
    model = model,
    param = param,
    info = info,
    data = data,
    extraResults = extraResults
  )

  amoeba(p = amoebaP, y = amoebaY, ndim = nParam, ftol = ftol, funk = FitLLTotal,
         deltaP,
         deltaM,
         theta,
         thetaP,
         noThetaFix,
         noDelta,
         modelSplineN,
         modelNoYears,
         modelYears,
         splineType,
         maxIncCorr,
         noEq,
         noStage,
         probSurv1996,
         model,
         param,
         info,
         data,
         extraResults)

}
