#include <Rcpp.h>

#ifndef _derivsMainFunc_
#define _derivsMainFunc_

using namespace Rcpp;

NumericVector derivsMainFunc(double x, NumericVector y, double lambda, int nVar, List param,
                             double year);

#endif // _derivsMainFunc_
