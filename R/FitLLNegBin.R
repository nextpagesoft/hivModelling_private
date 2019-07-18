FitLLNegBin.R <- function(
  y_m,
  y_d,
  r
) {
  LL <- rep(0, length(y_m))
  sel0 <- y_d == 0.0
  LL[sel0] <- -2 * r * (log(r) - log(r + y_m[sel0]))

  x_d <- y_d[!sel0]
  x_m <- y_m[!sel0]

  LL[!sel0] <- 2 * y_d * (log(x_d) - log(x_m)) - 2 * (r + x_d) * (log(r + x_d) - log(r + x_m))

  return(LL)
}
