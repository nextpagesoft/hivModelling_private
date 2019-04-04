#' PerformMainFit
#'
#' Description
#'
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#'
#' @return
#' model data.table object
#'
#' @examples
#' \dontrun{
#' PerformMainFit(context)
#' }
#'
#' @export
PerformMainFit <- function(context, data)
{
  model <- data.table()

  country <- context$Parameters$Models$INCIDENCE$Country

  # DEBUG

  # Number of disease stages
  noStage <- 5

  # Rate of progression to AIDS through stages of CD4
  #   type = 2 : Lodi et al, CID 2011, 53:817-825
  #   CASCADE Lancet 2000, 355:1131-37, Table 2.
  #   Cori et al, PLoS One 2014, 9(1):e84511, supplement
  modelCD4Rate <- 2

  maxNoFit <- 100

  # Number of time intervals diagnosis matrix
  modelNoTime <- 7

  modelMinYear <- 1980
  modelMaxYear <- 2017

  modelNoYears <- modelMaxYear - modelMinYear + 1

  # Background mortality
  mu <- 0

  alphaP <- 1.0/(2.90/12)

  fInit <- c(0.58, 0.23, 0.16, 0.03, 0)
  fInit[1] <- 1 - sum(fInit[-1])
  qoppa <- c(1/6.37, 1/2.86, 1/3.54, 1/2.3, 0.529101)
  delta4Fac <- 0

  # Time intervals
  tc <- c(1980, 1984, 1984, 1996, 2000, 2005, 2010, 2017)


  noEq <- 1 + noStage + noStage + noStage + 1 + 1 + 1 + 1 + 1
  modelNoIter <- maxNoFit
  startRandom <- FALSE
  iRun <- 0

  # Param_Prob_Surv
  probSurv1996 <- GetProvSurv96(country, noStage, qoppa, modelMinYear, modelNoYears)

  return(model)
}
