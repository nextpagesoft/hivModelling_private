#' GetParamList
#'
#' Get \code{param} list object.
#'
#' @param context List of parameters. Required.
#' @param info List of info parameters. Required.
#'
#' @return
#' list
#'
#' @examples
#' \dontrun{
#' GetParamList(context, info)
#' }
#'
#' @export
GetParamList <- function(
  context,
  info
) {
  incidenceParams <- context$Parameters$INCIDENCE

  param <- list(
    NoStage = incidenceParams$NoStage,
    FInit = incidenceParams$FInit,
    Mu = incidenceParams$Mu,
    AlphaP = incidenceParams$AlphaP,
    Qoppa = incidenceParams$Qoppa,
    Delta4Fac = incidenceParams$Delta4Fac,
    DeltaAIDS = incidenceParams$DeltaAIDS,
    NoThetaFix = incidenceParams$NoThetaFix,
    NoThetaFixInit = 0,
    Smoothing1 = incidenceParams$Smoothing1,
    Smoothing2 = incidenceParams$Smoothing2,
    RDispAIDS = incidenceParams$RDisp,
    RDispRest = incidenceParams$RDisp,
    DefNoDiagTime = incidenceParams$DefNoDiagTime,
    ChiSqDiff = incidenceParams$ChiSqDiff
  )

  # Time intervals for diagnosis matrix
  res <- GetDeltaPAndTc(
    intervals = incidenceParams$Intervals,
    maxYear = incidenceParams$ModelMaxYear,
    noStage = incidenceParams$NoStage
  )
  tc <- res$Tc
  deltaP <- res$DeltaP
  noTime <- length(tc) - 1

  noDelta <- max(max(deltaP))
  beta <- rep(0.2, noDelta)

  deltaM <- matrix(0, param$NoStage, noTime)
  deltaM[deltaP != 0] <- beta[deltaP]
  deltaM[param$NoStage, ] <- param$DeltaAIDS

  param[['Theta']] <- rep(0, info$ModelSplineN)
  param <- UpdateThetaParams(info, param)

  thetaF <- rep(100, sum(param$ThetaP))

  param[['NoEq']] <- 6 + 3 * param$NoStage
  param[['Tc']] <- tc
  param[['DeltaP']] <- deltaP
  param[['DeltaM']] <- deltaM
  param[['NoDelta']] <- noDelta
  param[['Beta']] <- beta
  param[['ThetaF']] <- thetaF
  param[['NoStageTot']] <- param$NoStage + 1


  return(param)
}
