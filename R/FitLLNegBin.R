FitLLNegBin <- function(
  y_m,
  y_d,
  r
) {
  LL <- ifelse(y_d == 0.0,
               -2 * r * (log(r) - log(r + y_m)),
               2 * y_d * (log(y_d) - log(y_m)) - 2 * (r + y_d) * (log(r + y_d) - log(r + y_m)))

  return(LL)
}
