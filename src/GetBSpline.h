#include <Rcpp.h>

#ifndef _GetBSpline_
#define _GetBSpline_

using namespace Rcpp;

double GetBSpline(double time, List param, List info, double minYear, double maxYear);

#endif // _GetBSpline_
