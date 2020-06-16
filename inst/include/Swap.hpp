#ifndef _hivModelling_Swap_
#define _hivModelling_Swap_

namespace hivModelling {

inline void Swap1D(
  Rcpp::NumericVector& y,
  const int a,
  const int b
) {
  const int a0 = a - 1;
  const int b0 = b - 1;

  const double s = y[a0];
  y[a0] = y[b0];
  y[b0] = s;
}

inline void Swap2D(
  Rcpp::NumericMatrix& y,
  const int a1,
  const Rcpp::IntegerVector& a2,
  const int b1,
  const Rcpp::IntegerVector& b2
) {
  const int a10 = a1 - 1;
  const Rcpp::IntegerVector a20 = a2 - 1;
  const int b10 = b1 - 1;
  const Rcpp::IntegerVector b20 = b2 - 1;

  for (int i = 0; i != a20.size(); ++i) {
    const double s = y(a10, a20[i]);
    y(a10, a20[i]) = y(b10, b20[i]);
    y(b10, b20[i]) = s;
  }
}

inline void DetermineIloIhi(
  const Rcpp::NumericVector& y,
  Rcpp::IntegerVector& ilo,
  Rcpp::IntegerVector& ihi,
  Rcpp::IntegerVector& inhi
) {
  const int n = y.length();
  for (int i = 0; i != n; ++i) {
    const int i1 = i + 1;

    if (y[i] <= y[ilo[0] - 1]) {
      ilo[0] = i1;
    }

    if (y[i] > y[ihi[0] - 1]) {
      inhi[0] = ihi[0];
      ihi[0] = i1;
    } else if (y[i] > y[inhi[0] - 1] && i1 != ihi[0]) {
      inhi[0] = i1;
    }
  }
}

} // hivModelling

#endif // _hivModelling_Swap_
