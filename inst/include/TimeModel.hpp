#ifndef _hivModelling_TimeModel_
#define _hivModelling_TimeModel_

#include "GetDelta.hpp"

namespace hivModelling {

inline Rcpp::NumericVector TimeModel(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::List& param,
  const double& year,
  Rcpp::NumericVector& dydx
) {
  const Rcpp::NumericVector& qoppa  = param["Qoppa"];
  const Rcpp::NumericVector& fInit  = param["FInit"];
  const double& alphaP              = param["AlphaP"];
  const double& mu                  = param["Mu"];
  const size_t& noStage             = param["NoStage"];
  const double& delta4Fac           = param["Delta4Fac"];
  const Rcpp::NumericMatrix& deltaM = param["DeltaM"];
  const Rcpp::NumericVector& tc     = param["Tc"];

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
