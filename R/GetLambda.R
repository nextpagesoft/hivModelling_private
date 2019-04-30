GetLambda <- function(
  time,
  sumInf,
  info
) {
  lambda <- 0

  if (time > info$TmpMinYear && time <= info$TmpMaxYear + 1e-7) {
    if (info$SplineType == 1) {
      stop('GetLambda for info$SplineType == 1 to be implemented')
    } else if (info$SplineType == 2) {
      lambda <- GetBSpline(time, kOrder = info$SplineOrder)
    } else if (info$SplineType == 3) {
      stop('GetLambda for info$SplineType == 3 to be implemented')
    }
  }

  return(lambda)
}
