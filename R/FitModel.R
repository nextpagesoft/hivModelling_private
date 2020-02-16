#' FitModel
#'
#' Fit model
#'
#' @param beta beta
#' @param theta theta
#' @param context context
#' @param data data
#'
#' @return list of outputs
#'
#' @examples
#' \dontrun{
#' FitModel(beta, theta, context, data)
#' }
#'
#' @export
FitModel <- function(beta, theta, context, data)
{
  info <- GetInfoList(context)
  param <- GetParamList(context, info)
  probSurv1996 <- GetProvSurv96(param, info)

  # Assuming all theta's are estimated
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
  param$ThetaP <- thetaP
  param$Theta <- theta
  param$NoTheta <- sum(thetaP)
  param$Beta <- beta
  param$ThetaF <- param$Theta[param$ThetaP != 0]

  p <- GetParameterVector(param$Beta, param$ThetaF)
  param$DeltaM <- GetParamDeltaM(p, param)

  fitResults <- FitLLTotal(p, probSurv1996, param, info, data)

  model <- modifyList(
    fitResults,
    list(
      Param = param,
      Info = info,
      ProbSurv1996 = probSurv1996
    )
  )

  return(model)
}
