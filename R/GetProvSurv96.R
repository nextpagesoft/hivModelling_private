#' GetProvSurv96
#'
#' Description
#'
#' @param param Parameters list
#' @param info Info list
#'
#' @return
#' matrix
#'
#' @examples
#' \dontrun{
#' GetProvSurv96(param, info)
#' }
#'
#' @export
GetProvSurv96 <- function(param, info)
{
  noStage <- param$NoStage
  qoppa <- param$Qoppa
  modelMinYear <- info$ModelMinYear
  modelNoYears <- info$ModelNoYears

  probSurv1996 <- matrix(1, modelNoYears, noStage)

  if (info$Country == 'NL') {
    qbar <- matrix(0, noStage, noStage)
    for (i in seq(noStage)) {
      for (j in seq(noStage)) {
        qbar[i, j] <- -qoppa[i] / (qoppa[j] - qoppa[i])
      }
    }
    qbar[is.infinite(qbar)] <- 1

    qbarC <- lapply(seq(noStage), function(i) {
      if (i != 5) {
        apply(qbar[i:5, i:5], 2, prod)
      } else {
        1
      }
    })

    for (i in seq(modelNoYears)) {
      year <- modelMinYear + i - 1

      if (year <= 1996) {
        at96 <- qoppa * (1996 - year)
        at96C <- exp(-at96)

        for (j in seq(noStage)) {
          probSurv1996[i, j] <- qbarC[[j]] %*% at96C[j:5]
        }
      }
    }
  }

  rownames(probSurv1996) <- seq(from = modelMinYear, length.out = modelNoYears)
  colnames(probSurv1996) <- sprintf('Stage %d', seq(noStage))

  return(probSurv1996)
}
