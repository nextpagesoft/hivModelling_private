#' GetParamList
#'
#' Get \code{param} list object.
#'
#' @param context List of parameters. Required.
#'
#' @return
#' list
#'
#' @examples
#' \dontrun{
#' GetParamList(context)
#' }
#'
#' @export
GetParamList <- function(
  context
) {
  incidenceParams <- context$Parameters$Models$INCIDENCE

  param <- list(
    NoStage = incidenceParams$NoStage,
    FInit = incidenceParams$FInit,
    Mu = incidenceParams$Mu,
    AlphaP = incidenceParams$AlphaP,
    Qoppa = incidenceParams$Qoppa,
    Delta4Fac = incidenceParams$Delta4Fac,
    DeltaAIDS = incidenceParams$DeltaAIDS,
    NoDelta = incidenceParams$NoDelta,
    NoTheta = incidenceParams$NoTheta,
    NoThetaFix = incidenceParams$NoThetaFix,
    Smoothing1 = incidenceParams$Smoothing1,
    Smoothing2 = incidenceParams$Smoothing2,
    RDispAIDS = incidenceParams$RDisp,
    RDispRest = incidenceParams$RDisp,
    DefNoDiagTime = incidenceParams$DefNoDiagTime
  )

  res <- GetDeltaPAndTc(incidenceParams$Intervals)

  # Time intervals for diagnosis matrix
  tc <- res$Tc
  deltaP <- res$DeltaP

  noTime <- length(tc) - 1

  deltaM <- matrix(0, param$NoStage, noTime)
  deltaM[param$NoStage, ] <- param$DeltaAIDS

  theta <- rep(0, param$NoTheta + 2)

  thetaP <- rep(0, param$NoTheta + 2)
  thetaP[2:(param$NoTheta + 1)] <- 1

  param[['NoEq']] <- 1 + param$NoStage + param$NoStage + param$NoStage + 1 + 1 + 1 + 1 + 1
  param[['Tc']] <- tc
  param[['DeltaP']] <- deltaP
  param[['DeltaM']] <- deltaM
  param[['ThetaP']] <- thetaP
  param[['Theta']] <- theta
  param[['NoStageTot']] <- param$NoStage + 1

  return(param)
}
