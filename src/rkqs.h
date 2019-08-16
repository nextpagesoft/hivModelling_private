#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _rkqs_
#define _rkqs_

using namespace Rcpp;

void rkqs(
  double x,
  NumericVector y,
  NumericVector dydx,
  int n,
  double htry,
  double eps,
  NumericVector yscal,
  List param,
  List info,
  double minYear,
  double maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear,
  List& rkqsRes,
  List& rkckRes
);

#endif // _rkqs_
