#include <Rcpp.h>

#ifndef _Models_
#define _Models_

using namespace Rcpp;

NumericVector CountModel(
  const double& x,
  const NumericVector& y,
  const double& lambda,
  const size_t& nVar,
  const List& param,
  const double& year,
  NumericVector& dydx
);

#endif // _Models_
