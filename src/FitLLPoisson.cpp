#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double FitLLPoisson(
    double y_m,
    double y_d
) {
  double LL = 0.0;

  if (y_d == 0.0) {
    LL = 2 * y_m;
  } else {
    LL = 2 * (y_d * (log(y_d) - log(y_m)) + y_m - y_d);
  }

  return LL;
}

/*** R
FitLLPoisson(y_m, y_d)
*/
