#include "header.h"

// [[Rcpp::export]]
Rcpp::NumericVector GetDelta(
  const double& time,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  const size_t& deadStageIdx
) {
  return hivModelling::GetDelta(time, delta4Fac, deltaM, tc, deadStageIdx);
}
