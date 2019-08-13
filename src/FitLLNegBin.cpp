#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector FitLLNegBin(
  NumericVector y_m,
  NumericVector y_d,
  double r
) {
  NumericVector LL(y_m.size());

  for (int i = 0; i < LL.size(); ++i) {
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
