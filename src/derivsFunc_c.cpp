#include <Rcpp.h>
#include "GetDelta_c.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector derivsFunc_c(
    double x,
    NumericVector y,
    double lambda,
    int nVar,
    List param
) {
  NumericVector dydx(nVar);
  NumericVector delta = GetDelta_c(x, param);
  NumericVector qoppa = param["Qoppa"];
  NumericVector fInit = param["FInit"];
  double alphaP = param["AlphaP"];
  double mu = param["Mu"];
  int noStage = param["NoStage"];

  double dlambdad2x = 0.0;

  int iEq = 0;

  // Element 0
  dydx[iEq] = lambda - (alphaP - mu) * y[iEq];
  iEq += 1;

  // Undiagnosed cases progressing through stages of infection
  dydx[iEq] = fInit[0] * alphaP * y[0] - (qoppa[0] + delta[0] + mu) * y[iEq];
  for (int i = 1; i < noStage; ++i) {
    int j = iEq + i;
    dydx[j] = fInit[i] * alphaP * y[0] + qoppa[i - 1] * y[j - 1] - (qoppa[i] + delta[i] + mu) * y[j];
  }
  iEq += noStage;

  // Diagnosed cases progressing through stages of infection i
  dydx[iEq] = delta[0] * y[1] - (qoppa[0] + mu) * y[iEq];
  for (int i = 1; i < noStage; ++i) {
    int j = iEq + i;
    dydx[j] = delta[i] * y[i + 1] + qoppa[i - 1] * y[j - 1] - (qoppa[i] + mu) * y[j];
  }
  iEq += noStage;

  // After diagnosed infection
  for (int i = 0; i < noStage; ++i) {
    int j = iEq + i;
    dydx[j] = delta[i] * y[1 + i];
  }
  iEq += noStage;

  // Cumulative number of AIDS cases
  dydx[iEq] = delta[noStage - 1] * y[noStage] + qoppa[noStage - 2] * y[2 * noStage - 1];
  iEq += 1;

  // Cumulative number of diagnosed deaths
  dydx[iEq] = qoppa[noStage - 1] * y[2 * noStage];
  for (int i = 0; i < noStage; ++i) {
    dydx[iEq] += mu * y[noStage + i];
  }
  iEq += 1;

  // Cumulative number of undiagnosed deaths
  dydx[iEq] = qoppa[noStage - 1] * y[noStage] + mu * y[0];
  for (int i = 0; i < noStage; ++i) {
    dydx[iEq] += mu * y[i];
  }
  iEq += 1;

  // Total cumulative incidence
  dydx[iEq] = lambda;
  iEq += 1;

  // After cumulative incidence (1 compartment) reset base counter
  dydx[iEq] = dlambdad2x * dlambdad2x;

  return dydx;
}

/*** R
# derivsFunc_c(x, y, lambda, nVar, param)
*/
