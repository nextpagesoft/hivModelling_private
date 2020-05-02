#ifndef _hivModelling_FitLLNegBin_
#define _hivModelling_FitLLNegBin_

namespace hivModelling {

inline Rcpp::NumericVector FitLLNegBin(
  const Rcpp::NumericVector& y_m,
  const Rcpp::NumericVector& y_d,
  const double& r
) {
  Rcpp::NumericVector LL(y_m.size());

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

} // hivModelling

#endif // _hivModelling_FitLLNegBin_
