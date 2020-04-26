#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _odeint_
#define _odeint_

using namespace Rcpp;

double odeint(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double tmpYear
);

#endif // _odeint_
