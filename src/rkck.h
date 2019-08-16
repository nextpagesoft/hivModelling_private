#include <Rcpp.h>
#include "hivModelling_types.h"
#include "GetBSpline.h"

#ifndef _rkck_
#define _rkck_

using namespace Rcpp;

void rkck(
  double x,
  NumericVector y,
  NumericVector dydx,
  int n,
  double h,
  List param,
  List info,
  double minYear,
  double maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear,
  List& result
);

#endif // _rkck_
