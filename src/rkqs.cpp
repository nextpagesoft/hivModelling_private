// Takes one "quality-controlled" Runge-Kutta step
#include "rkck.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rkqs(
  double x,
  NumericVector y,
  NumericVector dydx,
  int n,
  double htry,
  double eps,
  NumericVector yscal,
  List param,
  List info,
  double minYear,
  double maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear,
  List& rkqsRes,
  List& rkckRes
) {
  static const double SAFETY = 0.9;
  static const double PSHRNK = -0.25;
  static const double ERRCON = 1.89e-4;
  static const double PGROW = -0.2;

  NumericVector yerr(n);
  NumericVector ytemp(n);

  double hNext = 0;
  double hDid = 0;
  double minLambda = 0;
  double h = htry;

  for (;;) {
    rkck(x, y, dydx, n, h, param, info, minYear, maxYear, derivsFunc, tmpYear, rkckRes);

    NumericVector yOut = rkckRes["YOut"];
    NumericVector yErr = rkckRes["YErr"];
    minLambda = fmin(minLambda, rkckRes["MinLambda"]);

    double errMax = fmax(max(abs(yErr / yscal)), 0) / eps;

    if (errMax > 1.0) {
      double hTemp = SAFETY * h * pow(errMax, PSHRNK);
      h = h >= 0.0 ? fmax(hTemp, 0.1 * h) : fmin(hTemp, 0.1 * h);
    } else {
      if (errMax > ERRCON) {
        hNext = SAFETY * h * pow(errMax, PGROW);
      } else {
        hNext = 5.0 * h;
      }
      hDid = h;
      x += hDid;
      y = yOut;
      break;
    }
  }

  rkqsRes["X"] = x;
  rkqsRes["Y"] = y;
  rkqsRes["MinLambda"] = minLambda;
  rkqsRes["hDid"] = hDid;
  rkqsRes["hNext"] = hNext;
}

/*** R
rkqs(
  x, y, dydx, n, htry, eps, yscal, param, info, minYear, maxYear, derivsFuncXptr, tmpYear, rkqsRes,
  rkckRes
)
*/
