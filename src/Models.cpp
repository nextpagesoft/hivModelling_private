#include <Rcpp.h>
#include "GetDelta.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector CountModel(
  const double& x,
  const NumericVector& y,
  const double& lambda,
  const size_t& nVar,
  const NumericVector& qoppa,
  const NumericVector& fInit,
  const double& alphaP,
  const double& mu,
  const size_t& noStage,
  const double& delta4Fac,
  const NumericMatrix& deltaM,
  const NumericVector& tc,
  NumericVector& dydx
) {
  const NumericVector delta = GetDelta(x, delta4Fac, deltaM, tc, noStage);

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

// [[Rcpp::export]]
NumericVector TimeModel(
  const double& x,
  const NumericVector& y,
  const List& param,
  const double& year,
  NumericVector& dydx
) {
  const NumericVector& qoppa  = param["Qoppa"];
  const NumericVector& fInit  = param["FInit"];
  const double& alphaP        = param["AlphaP"];
  const double& mu            = param["Mu"];
  const size_t& noStage       = param["NoStage"];
  const double& delta4Fac     = param["Delta4Fac"];
  const NumericMatrix& deltaM = param["DeltaM"];
  const NumericVector& tc     = param["Tc"];

  const NumericVector delta = GetDelta(year, delta4Fac, deltaM, tc, noStage);

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
