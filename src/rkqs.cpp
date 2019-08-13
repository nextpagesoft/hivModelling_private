// Takes one "quality-controlled" Runge-Kutta step

#include <Rcpp.h>
#include "hivModelling_types.h"
#include "rkck.h"

using namespace Rcpp;

// [[Rcpp::export]]
List rkqs(
  double x,
  NumericVector y,
  NumericVector dydx,
  int n,
  double htry,
  double eps,
  NumericVector yscal,
  List param,
  List info,
  int minYear,
  int maxYear,
  DerivsFuncXPtr derivsFunc,
  int tmpYear
) {
  const double SAFETY = 0.9;
  const double PSHRNK = -0.25;
  const double ERRCON = 1.89e-4;
  const double PGROW = -0.2;

  NumericVector yerr(n);
  NumericVector ytemp(n);

  double hNext = 0;
  double hDid = 0;
  double minLambda = 0;
  double h = htry;
  for (;;) {
    List res = rkck(x, y, dydx, n, h, param, info, minYear, maxYear, derivsFunc, tmpYear);

    NumericVector yOut = res["YOut"];
    NumericVector yErr = res["YErr"];
    minLambda = fmin(minLambda, res["MinLambda"]);

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

  List result = List::create(
    Named("X") = x,
    Named("Y") = y,
    Named("MinLambda") = minLambda,
    Named("hDid") = hDid,
    Named("hNext") = hNext
  );

  return result;
}

/*** R
rkqs(x, y, dydx, n, htry, eps, yscal, param, info, minYear, maxYear, derivsFuncXptr, tmpYear)
*/
