#ifndef _hivModelling_CountModel_
#define _hivModelling_CountModel_

#include "GetDelta.hpp"

namespace hivModelling {

inline Rcpp::NumericVector CountModel(
  const double& x,
  const Rcpp::NumericVector& y,
  const double& lambda,
  const size_t& nVar,
  const Rcpp::NumericVector& qoppa,
  const Rcpp::NumericVector& fInit,
  const double& alphaP,
  const double& mu,
  const size_t& noStage,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  Rcpp::NumericVector& dydx
) {
  const Rcpp::NumericVector delta = GetDelta(x, delta4Fac, deltaM, tc, noStage);

  // Element 0
  dydx[0] = lambda - (alphaP - mu) * y[0];

  // Undiagnosed cases progressing through stages of infection
  int undiagIdx = 1;
  dydx[undiagIdx] = fInit[0] * alphaP * y[0] - (qoppa[0] + delta[0] + mu) * y[undiagIdx];

  // Diagnosed cases progressing through stages of infection i
  int diagIdx = undiagIdx + noStage;
  dydx[diagIdx] = delta[0] * y[1] - (qoppa[0] + mu) * y[diagIdx];

  // After diagnosed infection
  const int diagInfIdx = diagIdx + noStage;

  // Cumulative number of AIDS cases
  const int aidsIdx = diagInfIdx + noStage;
  dydx[aidsIdx] = delta[noStage - 1] * y[noStage] + qoppa[noStage - 2] * y[2 * noStage - 1];

  // Cumulative number of diagnosed deaths
  const int cumDiagDeathIdx = aidsIdx + 1;
  dydx[cumDiagDeathIdx] = qoppa[noStage - 1] * y[2 * noStage];

  // Cumulative number of undiagnosed deaths
  const int cumUndiagDeathIdx = cumDiagDeathIdx +  1;
  dydx[cumUndiagDeathIdx] = qoppa[noStage - 1] * y[noStage] + mu * y[0];

  // Progress through stages
  for (size_t i = 0; i != noStage; ++i) {
    if (i != 0) {
      undiagIdx++;
      dydx[undiagIdx] =
        fInit[i] * alphaP * y[0] + qoppa[i - 1] * y[undiagIdx - 1] -
        (qoppa[i] + delta[i] + mu) * y[undiagIdx];

      diagIdx++;
      dydx[diagIdx] =
        delta[i] * y[i + 1] + qoppa[i - 1] * y[diagIdx - 1] - (qoppa[i] + mu) * y[diagIdx];
    }

    dydx[diagInfIdx + i] = delta[i] * y[i + 1];

    dydx[cumDiagDeathIdx] += mu * y[noStage + i];

    dydx[cumUndiagDeathIdx] += mu * y[i];
  }

  // Total cumulative incidence
  dydx[cumUndiagDeathIdx + 1] = lambda;

  return dydx;
}

} // namespace

#endif // _hivModelling_CountModel_
