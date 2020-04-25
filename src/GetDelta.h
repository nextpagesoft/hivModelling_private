#include <Rcpp.h>

#ifndef _GetDelta_
#define _GetDelta_

using namespace Rcpp;

NumericVector GetDelta(const double& time, const List& param);

#endif // _GetDelta_
