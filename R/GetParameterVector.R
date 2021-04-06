#' GetParameterVector
#'
#' Get \code{param} vector initialized from inputs
#'
#' @param beta Numeric vector
#' @param thetaF Numeric vector
#' @param param Param list
#'
#' @return
#' numeric vector
#'
#' @examples
#' \dontrun{
#' GetParameterVector(beta, thetaF)
#' }
#'
#' @export
GetParameterVector <- function(
  beta,
  thetaF,
  param
) {
  p <- c(beta, thetaF)
  return(p)
}
