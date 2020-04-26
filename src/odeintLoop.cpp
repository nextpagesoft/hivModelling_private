#include <Rcpp.h>
#include "globals.h"
#include "odeint.h"

using namespace Rcpp;

// [[Rcpp::export]]
List odeintLoop(
  const NumericVector& modelYears,
  const List& param,
  const List& info,
  const DerivsFuncXPtr& derivsFunc
) {
  const size_t& nVar = param["NoEq"];
  const size_t& modelNoYears = as<int>(info["ModelNoYears"]) - 1;
  const double& modelMinYear = info["ModelMinYear"];
  const double& modelMaxYear = info["ModelMaxYear"];

  NumericVector ystart(nVar);
  NumericMatrix modelResults(modelNoYears, nVar);
  double minLambda = 1e+10;

  for (size_t i = 0; i != modelNoYears; ++i) {
    double resMinLambda = odeint(
      ystart, nVar, modelYears[i] + BIT_SML, modelYears[i + 1] - BIT_SML, param, info,
      modelMinYear, modelMaxYear, derivsFunc, 0
    );
    minLambda = fmin(minLambda, resMinLambda);
    modelResults(i, _) = ystart;
  }

  List result = List::create(
    Named("ModelResults") = modelResults,
    Named("MinLambda") = minLambda
  );

  return result;
}
