#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _rkqs_
#define _rkqs_

using namespace Rcpp;

void rkqs(
  double& x,
  NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& htry,
  const double& eps,
  const NumericVector& yscal,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double& tmpYear,
  List& rkqsRes,
  List& rkckRes
);

#endif // _rkqs_
