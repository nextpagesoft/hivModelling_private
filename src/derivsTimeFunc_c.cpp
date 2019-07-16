#include <Rcpp.h>
#include "GetDelta_c.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector derivsTimeFunc_c(
  double x,
  NumericVector y,
  double lambda,
  int nVar,
  List param,
  double year
) {
  NumericVector dydx(nVar);
  NumericVector delta = GetDelta_c(year, param);
  NumericVector qoppa = param["Qoppa"];
  NumericVector fInit = param["FInit"];
  double alphaP = param["AlphaP"];
  double mu = param["Mu"];
  int noStage = param["NoStage"];

  int iEq = 0;

  // Primary infection
  dydx[iEq] = -alphaP * y[0] - mu * y[0];
  iEq += 1;

  // Undiagnosed cases progressing through stages of infection
  dydx[iEq] = fInit[0] * alphaP * y[0] - (qoppa[0] + delta[0] + mu) * y[iEq];
  for (int i = 1; i < noStage; i++) {
    int j = iEq + i;
    dydx[j] = fInit[i] * alphaP * y[0] + qoppa[i - 1] * y[j - 1] - (qoppa[i] + delta[i] + mu) * y[j];
  }
  iEq += noStage;

  // Cumulative number of diagnoses in each stage
  for (int i = 0; i < noStage; i++) {
    int j = iEq + i;
    dydx[j] = delta[i] * y[j - noStage];
  }

  return dydx;
}

/*** R
# derivsTimeFunc_c(x, y, lambda, nVar, param, year)
*/
