#include <Rcpp.h>

#ifndef _Models_
#define _Models_

using namespace Rcpp;

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
);

NumericVector TimeModel(
  const double& x,
  const NumericVector& y,
  const List& param,
  const double& year,
  NumericVector& dydx
);

#endif // _Models_
