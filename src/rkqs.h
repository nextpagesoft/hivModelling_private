#include <Rcpp.h>
#include "hivModelling_types.h"

#ifndef _rkqs_
#define _rkqs_

using namespace Rcpp;

List rkqs(
    double x,
    NumericVector y,
    NumericVector dydx,
    int n,
    double htry,
    double eps,
    NumericVector yscal,
    List param,
    List info,
    int minYear,
    int maxYear,
    DerivsFuncXPtr derivsFunc,
    int tmpYear
);

#endif // _rkqs_
