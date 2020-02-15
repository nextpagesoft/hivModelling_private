#' GetParameterVector
#'
#' Get \code{param} vector initialized from inputs
#'
#' @param beta Numeric vector
#' @param thetaF Numeric vector
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
  thetaF
) {
  p <- c(beta, thetaF)
  # p <- rep(0, param$NoDelta + param$NoTheta)
  # p[seq_len(param$NoDelta)] <- beta[seq_len(param$NoDelta)]
  # p[param$NoDelta + seq_len(param$NoTheta)] <- thetaF[seq_len(param$NoTheta)]
  return(p)
}
