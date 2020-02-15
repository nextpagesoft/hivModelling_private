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
  param$Theta <- theta
  param$NoTheta <- length(param$Theta)
  param$ThetaP <- rep(1, param$NoTheta)

  thetaF <- param$Theta[param$ThetaP != 0]
  p <- GetParameterVector(beta, thetaF)
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
