#ifndef _hivModelling_CountRkqs_
#define _hivModelling_CountRkqs_

#include "globals.hpp"
#include "CountRkck.hpp"

namespace hivModelling {

inline void CountRkqs(
  double& x,
  Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& htry,
  const double& eps,
  const Rcpp::NumericVector& yscal,
  const double& minYear,
  const double& maxYear,
  double& rkqsLambda,
  double& hDid,
  double& hNext,
  double& rkckLambda,
  Rcpp::NumericVector& yOut,
  Rcpp::NumericVector& yErr
) {
  rkqsLambda = 0;
  rkckLambda = 0;
  double h = htry;

  for (;;) {
    CountRkck(x, y, dydx, nVar, h, minYear, maxYear, rkckLambda, yOut, yErr);

    rkqsLambda = fmin(rkqsLambda, rkckLambda);

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
      y = clone(yOut);
      break;
    }
  }
}

} // hivModelling

#endif // _hivModelling_CountRkqs_
