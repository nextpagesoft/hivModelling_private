#include <Rcpp.h>

using namespace Rcpp;

#ifndef _hivModelling_types_
#define _hivModelling_types_

typedef NumericVector (*derivsFuncPtr)(
  const double& x,
  const NumericVector& y,
  const double& lambda,
  const size_t& nVar,
  const List& param,
  const double& year,
  NumericVector& dydx
);

typedef XPtr<derivsFuncPtr> DerivsFuncXPtr;

#endif // _hivModelling_types_
