#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double FitLLNegBin(
    double y_m,
    double y_d,
    double r
) {
  double LL = 0.0;

  if (y_d == 0.0) {
    LL = -2 * r *(log(r) - log(r + y_m));
  } else {
    LL = 2 * y_d *(log(y_d) - log(y_m)) - 2 * (r + y_d) * (log(r + y_d) - log(r + y_m));
  }

  return LL;
}

/*** R
FitLLNegBin(y_m, y_d, r)
*/
