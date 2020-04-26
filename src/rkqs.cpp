// Takes one "quality-controlled" Runge-Kutta step
#include "rkqs.h"
#include "globals.h"
#include "rkck.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rkqs(
  double& x,
  NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& htry,
  const double& eps,
  const NumericVector& yscal,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double& tmpYear,
  List& rkqsRes,
  List& rkckRes
) {
  NumericVector yerr(nVar);
  NumericVector ytemp(nVar);

  double hNext = 0;
  double hDid = 0;
  double minLambda = 0;
  double h = htry;

  for (;;) {
    rkck(x, y, dydx, nVar, h, param, info, minYear, maxYear, derivsFunc, tmpYear, rkckRes);

    const NumericVector& yOut = rkckRes["YOut"];
    const NumericVector& yErr = rkckRes["YErr"];
    minLambda = fmin(minLambda, as<double>(rkckRes["MinLambda"]));

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

  rkqsRes["MinLambda"] = minLambda;
  rkqsRes["hDid"] = hDid;
  rkqsRes["hNext"] = hNext;
}
