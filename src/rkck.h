#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _rkck_
#define _rkck_

using namespace Rcpp;

void rkck(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double& tmpYear,
  List& result
);

#endif // _rkck_
