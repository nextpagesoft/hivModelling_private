#include <Rcpp.h>
#include "Sign.h"

using namespace Rcpp;

// [[Rcpp::export]]
double zbrent(
  Function func,
  double x1,
  double x2,
  double tol,
  List extraArgs
) {
  const int ITMAX = 100;
  const double EPS = 3.0e-8;

  double a = x1;
  double b = x2;
  double c = x2;
  double d = 0.0;
  double e = 0.0;
  double fa = as<double>(func(a, extraArgs));
  double fb = as<double>(func(b, extraArgs));

  if ((fa > 0.0 && fb > 0.0) || (fa < 0.0 && fb < 0.0)) {
    Rcout << "zbrent: Root must be bracketed\n";
  }

  double fc = fb;

  for (int iter = 1; iter <= ITMAX; iter++) {
    if ((fb > 0.0 && fc > 0.0) || (fb < 0.0 && fc < 0.0)) {
      c = a;
      fc = fa;
      e = d = b - a;
    }

    if (fabs(fc) < fabs(fb)) {
      a = b;
      b = c;
      c = a;
      fa = fb;
      fb = fc;
      fc = fa;
    }

    double tol1 = 2.0 * EPS * fabs(b) + 0.5 * tol;
    double xm = 0.5 * (c - b);

    if (fabs(xm) <= tol1 || fb == 0.0) {
      return b;
    }

    if (fabs(e) >= tol1 && fabs(fa) > fabs(fb)) {
      double s = fb / fa;
      double p = 0.0;
      double q = 0.0;
      double r = 0.0;
      if (a == c) {
        p = 2.0 * xm * s;
        q = 1.0 - s;
      } else {
        q = fa / fc;
        r = fb / fc;
        p = s * (2.0 * xm * q * (q - r) - (b - a) * (r - 1.0));
        q = (q - 1.0) * (r - 1.0) * (s - 1.0);
      }
      if (p > 0.0) {
        q = -q;
      }

      p = fabs(p);
      double min1 = 3.0 * xm * q - fabs(tol1 * q);
      double min2 = fabs(e * q);
      if (2.0 * p < (min1 < min2 ? min1 : min2)) {
        e = d;
        d = p / q;
      } else {
        d = xm;
        e = d;
      }
    } else {
      d = xm;
      e = d;
    }
    a = b;
    fa = fb;
    if (fabs(d) > tol1) {
      b += d;
    } else {
      b += Sign(tol1, xm);
    }

    fb = as<double>(func(b, extraArgs));
  }

  Rcout << "Maximum number of iterations exceeded in zbrent";

  return 0.0;
}


/*** R
# zbrent(42)
*/
