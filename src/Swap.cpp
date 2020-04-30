#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void Swap1D(Rcpp::NumericVector& y, const int a, const int b) {
  const int a0 = a - 1;
  const int b0 = b - 1;

  const double s = y[a0];
  y[a0] = y[b0];
  y[b0] = s;
}

// [[Rcpp::export]]
void Swap2D(Rcpp::NumericMatrix& y, const int a1, const int a2, const int b1, const int b2) {
  const int a10 = a1 - 1;
  const int a20 = a2 - 1;
  const int b10 = b1 - 1;
  const int b20 = b2 - 1;

  const double s = y(a10, a20);
  y(a10, a20) = y(b10, b20);
  y(b10, b20) = s;
}
