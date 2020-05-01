#include <Rcpp.h>

#ifndef _GetDelta_
#define _GetDelta_

using namespace Rcpp;

NumericVector GetDelta(
  const double& time,
  const double& delta4Fac,
  const NumericMatrix& deltaM,
  const NumericVector& tc,
  const size_t& deadStageIdx
);

#endif // _GetDelta_
