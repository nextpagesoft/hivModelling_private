#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double Sign(double a, double b) {
  return b >= 0.0 ? fabs(a) : -fabs(a);
}

/*** R
Sign(10, -1)
*/
