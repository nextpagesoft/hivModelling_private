#ifndef _hivModelling_CountOdeintLoop_
#define _hivModelling_CountOdeintLoop_

#include "globals.hpp"
#include "CountModelParameters.hpp"
#include "CountOdeint.hpp"

namespace hivModelling {

inline Rcpp::List CountOdeintLoop(
  const Rcpp::NumericVector& modelYears
) {
  Rcpp::NumericVector ystart(nVar);
  Rcpp::NumericMatrix modelResults(modelNoYears, nVar);
  double minLambda = 1e+10;

  for (size_t i = 0; i != modelNoYears; ++i) {
    double resMinLambda = CountOdeint(
      ystart, nVar, modelYears[i] + BIT_SML, modelYears[i + 1] - BIT_SML, modelMinYear, modelMaxYear
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

#endif // _hivModelling_CountOdeintLoop_
