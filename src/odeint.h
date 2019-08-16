#include <Rcpp.h>
#include "hivModelling_types.h"
#include "Sign.h"
#include "GetBSpline.h"
#include "rkqs.h"

#ifndef _odeint_
#define _odeint_

using namespace Rcpp;

List odeint(
  NumericVector ystart,
  int nVar,
  double x1,
  double x2,
  double eps,
  double h1,
  List param,
  List info,
  double minYear,
  double maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear
);

#endif // _odeint_
