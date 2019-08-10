#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _rkck_
#define _rkck_

using namespace Rcpp;

List rkck(
    double x,
    NumericVector y,
    NumericVector dydx,
    int n,
    double h,
    List param,
    List info,
    int minYear,
    int maxYear,
    DerivsFuncXPtr derivsFunc,
    double tmpYear
);

#endif // _rkck_
