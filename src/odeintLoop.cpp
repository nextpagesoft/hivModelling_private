#include <Rcpp.h>
#include "odeint.h"

using namespace Rcpp;

// [[Rcpp::export]]
List odeintLoop(
  NumericVector modelYears,
  List param,
  List info,
  DerivsFuncXPtr derivsFunc
) {
  static const double bitSml = 1e-6;
  static const double eps = 1e-4;
  static const double h1 = 0.02;

  const int nVar = param["NoEq"];
  const int modelNoYears = as<int>(info["ModelNoYears"]) - 1;
  const double modelMinYear = info["ModelMinYear"];
  const double modelMaxYear = info["ModelMaxYear"];

  NumericVector ystart(nVar);
  NumericMatrix modelResults(modelNoYears, nVar);
  double minLambda = 1e+10;

  for (int i = 0; i != modelNoYears; ++i) {
    List res = odeint(ystart,
                      nVar,
                      modelYears[i] + bitSml,
                      modelYears[i + 1] - bitSml,
                      eps,
                      h1,
                      param,
                      info,
                      modelMinYear,
                      modelMaxYear,
                      derivsFunc,
                      0);
    ystart = res["YStart"];
    double resMinLambda = res["MinLambda"];
    minLambda = fmin(minLambda, resMinLambda);
    modelResults(i, _) = ystart;
  }

  List result = List::create(
    Named("ModelResults") = modelResults,
    Named("MinLambda") = minLambda
  );

  return result;
}

/*** R
odeintLoop(modelYears, param, info, derivsFunc)
*/
