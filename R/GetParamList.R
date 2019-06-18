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
    NoTime = incidenceParams$NoTime,
    Mu = incidenceParams$Mu,
    AlphaP = incidenceParams$AlphaP,
    Qoppa = incidenceParams$Qoppa,
    Delta4Fac = incidenceParams$Delta4Fac,
    DeltaAIDS = incidenceParams$DeltaAIDS,
    Tc = incidenceParams$Tc,
    NoDelta = incidenceParams$NoDelta,
    NoTheta = incidenceParams$NoTheta,
    NoThetaFix = incidenceParams$NoThetaFix,
    AutoThetaFix = ifelse(incidenceParams$FullData, 0L, 1L),
    Smoothing1 = incidenceParams$Smoothing1,
    Smoothing2 = incidenceParams$Smoothing2,
    RDispAIDS = incidenceParams$RDisp,
    RDispRest = incidenceParams$RDisp,
    DefNoDiagTime = incidenceParams$DefNoDiagTime
  )

  deltaM <- matrix(0, param$NoStage, param$NoTime)
  deltaM[param$NoStage, ] <- param$DeltaAIDS
  deltaM[1:4, 2] <- 0.2

  theta <- rep(0, param$NoTheta + 2)

  deltaP <- matrix(0, param$NoStage, param$NoTime)
  deltaP[1:4, 2] <- 1

  thetaP <- rep(0, param$NoTheta + 2)
  thetaP[2:(param$NoTheta + 1)] <- 1

  param[['NoEq']] <-
    1 + param$NoStage + param$NoStage + param$NoStage + 1 + 1 + 1 + 1 + 1
  param[['DeltaP']] <- deltaP
  param[['ThetaP']] <- thetaP
  param[['DeltaM']] <- deltaM
  param[['Theta']] <- theta
  param[['NoStageTot']] <- param$NoStage + 1

  return(param)
}
