FitLLPoisson <- function(
  y_m,
  y_d
) {
  LL <- ifelse(y_d == 0.0,
               2 * y_m,
               2 * (y_d * (log(y_d) - log(y_m)) + y_m - y_d))

  return(LL)
}
