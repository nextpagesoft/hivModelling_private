#ifndef _hivModelling_TimeModel_
#define _hivModelling_TimeModel_

#include "GetDelta.hpp"

namespace hivModelling {

inline Rcpp::NumericVector TimeModel(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::NumericVector& qoppa,
  const Rcpp::NumericVector& fInit,
  const double& alphaP,
  const double& mu,
  const size_t& noStage,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  const double& year,
  Rcpp::NumericVector& dydx
) {
  const Rcpp::NumericVector delta = GetDelta(year, delta4Fac, deltaM, tc, noStage);

  size_t j = 0;
  size_t iEq = 0;

  // Primary infection
  dydx[iEq] = -alphaP * y[0] - mu * y[0];
  iEq += 1;

  // Undiagnosed cases progressing through stages of infection
  dydx[iEq] = fInit[0] * alphaP * y[0] - (qoppa[0] + delta[0] + mu) * y[iEq];
  for (size_t i = 1; i != noStage; ++i) {
    j = iEq + i;
    dydx[j] = fInit[i] * alphaP * y[0] + qoppa[i - 1] * y[j - 1] - (qoppa[i] + delta[i] + mu) * y[j];
  }
  iEq += noStage;

  // Cumulative number of diagnoses in each stage
  for (size_t i = 0; i != noStage; ++i) {
    j = iEq + i;
    dydx[j] = delta[i] * y[j - noStage];
  }

  return dydx;
}

} // namespace

#endif // _hivModelling_TimeModel_
