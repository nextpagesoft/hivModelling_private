#' GetParameterVector
#'
#' Get \code{param} vector initialized from inputs
#'
#' @param beta Numeric vector
#' @param thetaF Numeric vector
#' @param param List of parameters
#'
#' @return
#' numeric vector
#'
#' @examples
#' \dontrun{
#' GetParameterVector(beta, thetaF, param)
#' }
#'
#' @export
GetParameterVector <- function(
  beta,
  thetaF,
  param
) {
  p <- rep(0, param$NoDelta + param$NoTheta)
  p[seq(param$NoDelta)] <- beta[seq(param$NoDelta)]
  p[param$NoDelta + seq(param$NoTheta)] <- thetaF[seq(param$NoTheta)]
  return(p)
}
