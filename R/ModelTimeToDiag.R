ModelTimeToDiag <- function(
  delta,
  param
) {
  timeToDiag <- 1 / param$AlphaP

  for (k in seq_len(param$NoStage)) {
    val3 <- 0

    for (j in seq(k, param$NoStage)) {

      val1 <- 0
      for (i in seq(k, j)) {
        val1 <- val1 + 1 / (param$Qoppa[i] + delta[i])
      }

      val2 <- 1
      if (k <= j - 1) {
        for (i in seq(k, j - 1)) {
          val2 <- val2  * param$Qoppa[i] / (param$Qoppa[i] + delta[i])
        }
      }

      val3 <- val3 + val1 * val2 * delta[j] / (param$Qoppa[j] + delta[j])
    }
    timeToDiag <- timeToDiag + param$FInit[k] * val3
  }

  return(timeToDiag)
}
