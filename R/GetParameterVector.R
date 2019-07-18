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
  p[seq_len(param$NoDelta)] <- beta[seq_len(param$NoDelta)]
  p[param$NoDelta + seq_len(param$NoTheta)] <- thetaF[seq_len(param$NoTheta)]
  return(p)
}
