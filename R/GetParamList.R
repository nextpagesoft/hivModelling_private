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
    Smoothing1 = incidenceParams$Smoothing1,
    Smoothing2 = incidenceParams$Smoothing2,
    RDispAIDS = incidenceParams$RDisp,
    RDispRest = incidenceParams$RDisp,
    DefNoDiagTime = incidenceParams$DefNoDiagTime
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

  # Initialize all thetaP at 1, i.e. no spline weight fixed
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

  theta <- rep(0, info$ModelSplineN)
  thetaF <- rep(0, sum(thetaP))

  param[['NoEq']] <- 1 + param$NoStage + param$NoStage + param$NoStage + 1 + 1 + 1 + 1 + 1
  param[['Tc']] <- tc
  param[['DeltaP']] <- deltaP
  param[['DeltaM']] <- deltaM
  param[['NoDelta']] <- noDelta
  param[['Beta']] <- beta
  param[['ThetaP']] <- thetaP
  param[['Theta']] <- theta
  param[['ThetaF']] <- thetaF
  param[['NoTheta']] <- sum(thetaP)
  param[['NoStageTot']] <- param$NoStage + 1

  return(param)
}
