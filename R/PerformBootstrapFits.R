#' PerformBootstrapFits
#'
#' Performs multiple bootstrap fits
#'
#' @param bsCount Count of boostrap iterations. Default = \code{20}
#' @param context List of parameters. Required.
#' @param data Input data as data.table. Required.
#' @param mainResults Main results
#' @param maxNoFit Maximum number of amoeba iterations. Optional. Default = 30.
#' @param ctol Minium required deviance in consecutive lambda estimations.
#'   Optional. Default = 1e-6.
#' @param ftol Minium required deviance in amoeba calculations. Optional.
#'   Default = 1e-5.
#' @param ... Additional arguments passed to amoeba function. Optional.
#'
#' @return
#' list object containing context, data and model objects
#'
#' @examples
#' \dontrun{
#' PerformBootstrapFits(bsCount, context, data, maxNoFit = 2, ctol = 1e-6, ftol = 1e-5, verbose)
#' }
#'
#' @export
PerformBootstrapFits <- function(
  bsCount = 20,
  context,
  data,
  mainResults,
  maxNoFit = 30,
  ctol = 1e-6,
  ftol = 1e-5,
  ...
) {
  results <- list()
  for (idx in seq_len(bsCount)) {
    results[[idx]] <- PerformBootstrapFit(context, data, mainResults, maxNoFit, ctol, ftol, ...)
  }

  return(results)
}
