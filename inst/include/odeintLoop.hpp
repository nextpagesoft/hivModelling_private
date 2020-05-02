#ifndef _hivModelling_odeintloop_
#define _hivModelling_odeintloop_

#include "globals.hpp"
#include "odeint.hpp"

namespace hivModelling {

inline Rcpp::List odeintLoop(
  const Rcpp::NumericVector& modelYears,
  const Rcpp::List& param,
  const Rcpp::List& info
) {
  const size_t& nVar = param["NoEq"];
  const size_t& modelNoYears = Rcpp::as<int>(info["ModelNoYears"]) - 1;
  const double& modelMinYear = info["ModelMinYear"];
  const double& modelMaxYear = info["ModelMaxYear"];

  Rcpp::NumericVector ystart(nVar);
  Rcpp::NumericMatrix modelResults(modelNoYears, nVar);
  double minLambda = 1e+10;

  for (size_t i = 0; i != modelNoYears; ++i) {
    double resMinLambda = odeint_count(
      ystart, nVar, modelYears[i] + BIT_SML, modelYears[i + 1] - BIT_SML, param, info,
      modelMinYear, modelMaxYear
    );
    minLambda = fmin(minLambda, resMinLambda);
    modelResults(i, Rcpp::_) = ystart;
  }

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("ModelResults") = modelResults,
    Rcpp::Named("MinLambda") = minLambda
  );

  return result;
}

} // hivModelling

#endif // _hivModelling_odeintloop_
