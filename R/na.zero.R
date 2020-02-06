na.zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}
