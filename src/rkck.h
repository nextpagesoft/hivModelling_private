#include <Rcpp.h>

#ifndef _rkck_
#define _rkck_

using namespace Rcpp;

void rkck_count(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  double& rkckLambda,
  NumericVector& yout,
  NumericVector& yerr
);

void rkck_time(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear,
  List& result
);

#endif // _rkck_
