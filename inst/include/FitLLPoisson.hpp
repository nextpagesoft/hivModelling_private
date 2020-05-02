#ifndef _hivModelling_FitLLPoisson_
#define _hivModelling_FitLLPoisson_

namespace hivModelling {

inline Rcpp::NumericVector FitLLPoisson(
  const Rcpp::NumericVector& y_m,
  const Rcpp::NumericVector& y_d
) {
  Rcpp::NumericVector LL(y_m.size());

  const size_t n = LL.size();
  for (size_t i = 0; i != n; ++i) {
    if (y_d[i] == 0.0) {
      LL[i] = 2 * y_m[i];
    } else {
      LL[i] = 2 * (y_d[i] * (log(y_d[i]) - log(y_m[i])) + y_m[i] - y_d[i]);
    }
  }

  return LL;
}

} // hivModelling

#endif // _hivModelling_FitLLPoisson_
