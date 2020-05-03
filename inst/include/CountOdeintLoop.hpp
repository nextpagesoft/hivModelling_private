#ifndef _hivModelling_CountOdeintLoop_
#define _hivModelling_CountOdeintLoop_

#include "globals.hpp"
#include "CountModelParameters.hpp"
#include "CountOdeint.hpp"
#include "Seq.hpp"

namespace hivModelling {

inline Rcpp::List CountOdeintLoop() {
  const Rcpp::IntegerVector modelYears = Seq(modelMinYear, modelMaxYear - 1);
  const size_t modelNoYears = modelYears.size();

  double minLambda = VERY_LRG;
  Rcpp::NumericVector yStart(nVar);
  Rcpp::NumericMatrix modelResults(modelNoYears, nVar + 1);
  for (size_t i = 0; i != modelNoYears; ++i) {
    double odeintLambda = CountOdeint(
      yStart, nVar, modelYears[i] + BIT_SML, modelYears[i] + 1 - BIT_SML, modelMinYear, modelMaxYear
    );

    modelResults(i, 0) = modelYears[i];
    for (size_t j = 0; j != nVar; ++j) {
      modelResults(i, j + 1) = yStart[j];
    }

    minLambda = fmin(minLambda, odeintLambda);
  }
  Rcpp::colnames(modelResults) = modelResultsColNames;

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("ModelResults") = modelResults,
    Rcpp::Named("MinLambda") = minLambda
  );

  return result;
}

} // hivModelling

#endif // _hivModelling_CountOdeintLoop_
