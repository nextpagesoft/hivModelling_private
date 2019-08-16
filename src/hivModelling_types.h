#include <Rcpp.h>

using namespace Rcpp;

#ifndef _hivModelling_types_
#define _hivModelling_types_

typedef NumericVector (*derivsFuncPtr)(
  double x,
  NumericVector y,
  double lambda,
  int nVar,
  List param,
  double year
);

typedef XPtr<derivsFuncPtr> DerivsFuncXPtr;

#endif // _hivModelling_types_
