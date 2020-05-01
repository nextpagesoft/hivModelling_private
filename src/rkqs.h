#include <Rcpp.h>

#ifndef _rkqs_
#define _rkqs_

using namespace Rcpp;

void rkqs_count(
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
  double& rkqsLambda,
  double& hDid,
  double& hNext,
  double& rkckLambda,
  NumericVector& yOut,
  NumericVector& yErr
);

void rkqs_time(
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
  const double& tmpYear,
  List& rkqsRes,
  List& rkckRes
);

#endif // _rkqs_
