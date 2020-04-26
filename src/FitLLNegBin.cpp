#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector FitLLNegBin(
  const NumericVector& y_m,
  const NumericVector& y_d,
  const double& r
) {
  NumericVector LL(y_m.size());

  const size_t n = LL.size();
  for (size_t i = 0; i != n; ++i) {
    if (y_d[i] == 0.0) {
      LL[i] = -2 * r *(log(r) - log(r + y_m[i]));
    } else {
      LL[i] =
        2 * y_d[i] *(log(y_d[i]) - log(y_m[i])) -
        2 * (r + y_d[i]) * (log(r + y_d[i]) - log(r + y_m[i]));
    }
  }

  return LL;
}

/*** R
FitLLNegBin(y_m, y_d, r)
*/
