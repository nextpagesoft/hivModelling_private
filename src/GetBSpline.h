#include <Rcpp.h>

#ifndef _GetBSpline_
#define _GetBSpline_

using namespace Rcpp;

double GetBSpline(
  double time, NumericVector theta, int kOrder, int modelSplineN, NumericVector myKnots,
  double minYear, double maxYear
);

#endif // _GetBSpline_
