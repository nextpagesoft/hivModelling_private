#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double Sign_c(double a, double b) {
  return b >= 0.0 ? fabs(a) : -fabs(a);
}

/*** R
Sign_c(10, -1)
*/
