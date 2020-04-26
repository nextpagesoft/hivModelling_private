#include <Rcpp.h>

#ifndef _GetBSpline_
#define _GetBSpline_

using namespace Rcpp;

double GetBSpline(
  const double& time,
  const NumericVector& theta,
  const size_t& kOrder,
  const size_t& modelSplineN,
  const NumericVector& myKnots,
  const double& minYear,
  const double& maxYear
);

#endif // _GetBSpline_
