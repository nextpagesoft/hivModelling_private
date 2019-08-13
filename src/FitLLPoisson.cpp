#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector FitLLPoisson(
  NumericVector y_m,
  NumericVector y_d
) {
  NumericVector LL(y_m.size());

  for (int i = 0; i < LL.size(); ++i) {
    if (y_d[i] == 0.0) {
      LL[i] = 2 * y_m[i];
    } else {
      LL[i] = 2 * (y_d[i] * (log(y_d[i]) - log(y_m[i])) + y_m[i] - y_d[i]);
    }
  }

  return LL;
}

/*** R
FitLLPoisson(y_m, y_d)
*/
