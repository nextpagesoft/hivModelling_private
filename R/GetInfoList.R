#' GetInfoList
#'
#' Get \code{info} list object.
#'
#' @param context List of parameters. Required.
#'
#' @return
#' list
#'
#' @examples
#' \dontrun{
#' GetInfoList(context)
#' }
#'
#' @export
GetInfoList <- function(
  context
) {
  VERY_SML <- 1e-20

  incidenceParams <- context$Parameters$Models$INCIDENCE

  info <- list(
    Country = incidenceParams$Country,
    ModelMinYear = incidenceParams$ModelMinYear,
    ModelMaxYear = incidenceParams$ModelMaxYear,
    FitPosMinYear = incidenceParams$FitPosMinYear,
    FitPosMaxYear = incidenceParams$FitPosMaxYear,
    FitPosCD4MinYear = incidenceParams$FitPosCD4MinYear,
    FitPosCD4MaxYear = incidenceParams$FitPosCD4MaxYear,
    FitAIDSPosMinYear = incidenceParams$FitAIDSPosMinYear,
    FitAIDSPosMaxYear = incidenceParams$FitAIDSPosMaxYear,
    FitAIDSMinYear = incidenceParams$FitAIDSMinYear,
    FitAIDSMaxYear = incidenceParams$FitAIDSMaxYear,

    ModelFitDist = 'POISSON',

    ModelNoKnots = incidenceParams$ModelNoKnots,

    SplineType = incidenceParams$SplineType,
    SplineOrder = incidenceParams$SplineOrder,
    MaxIncCorr = incidenceParams$MaxIncCorr
  )

  message('Input distribution was set to "', incidenceParams$FitDistribution, '. ',
          'This is overridden to "POISSON".')

  info[['ModelNoYears']] <- info$ModelMaxYear - info$ModelMinYear + 1
  info[['ModelSplineN']] <- info$ModelNoKnots + info$SplineOrder

  # Param_Knots
  knotsDistance <-
    (info$ModelMaxYear - info$ModelMinYear) / (info$ModelNoKnots + 1)

  knots <- matrix(0,
                  info$SplineOrder + 1,
                  info$ModelNoKnots + 2 * (info$SplineOrder + 1))
  myKnots <- NULL
  for (k in seq_len(info$SplineOrder + 1)) {
    knots[k, 1:k] <- info$ModelMinYear
    knots[k, (k + 1):(k + info$ModelNoKnots)] <-
      info$ModelMinYear +
      ((k + 1):(k + info$ModelNoKnots) - k) * knotsDistance
    knots[k, (k + info$ModelNoKnots + 1):(info$ModelNoKnots + 2 * k)] <-
      info$ModelMaxYear + VERY_SML

    if (k == info$SplineOrder) {
      myKnots <- knots[k, seq_len(2 * info$SplineOrder + info$ModelNoKnots)]
    }
  }

  info[['Knots']] <- knots
  info[['MyKnots']] <- myKnots

  return(info)
}
