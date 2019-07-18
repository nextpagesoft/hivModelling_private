FitLLPoisson.R <- function(
  y_m,
  y_d
) {
  if (y_d == 0.0) {
    LL <- 2 * y_m
  } else {
    LL <- 2 * (y_d * (log(y_d) - log(y_m)) + y_m - y_d)
  }

  # LL <- rep(0, length(y_m))
  # sel0 <- y_d == 0.0
  # LL[sel0] <- 2 * y_m[sel0]
  #
  # x_d <- y_d[!sel0]
  # x_m <- y_m[!sel0]
  #
  # LL[!sel0] <- 2 * (x_d * (log(x_d) - log(x_m)) + x_m - x_d)
  #
  return(LL)
}
