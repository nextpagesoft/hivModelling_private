#ifndef _hivModelling_CountOdeint_
#define _hivModelling_CountOdeint_

#include "Sign.hpp"
#include "CountModelParameters.hpp"
#include "CountRkqs.hpp"

namespace hivModelling {

inline double CountOdeint(
  Rcpp::NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const double& minYear,
  const double& maxYear
) {
  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(H1, x2 - x1);

  Rcpp::NumericVector y = clone(ystart);

  double minLambda = VERY_LRG;
  double rkqsLambda = 0;
  double hDid = 0;
  double hNext = 0;
  double rkckLambda = 0;
  Rcpp::NumericVector yOut(nVar);
  Rcpp::NumericVector yErr(nVar);

  double derivLambda;
  Rcpp::NumericVector dydx(nVar);
  Rcpp::NumericVector yscal(nVar);

  for (int nstp = 0; nstp != MAX_STP; ++nstp) {

    derivLambda = GetBSpline(x, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
    CountModel(
      x, y, derivLambda, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, dydx
    );

    yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    CountRkqs(
      x, y, dydx, nVar, h, EPS, yscal, minYear, maxYear, rkqsLambda, hDid, hNext,
      rkckLambda, yOut, yErr
    );

    minLambda = fmin(fmin(minLambda, derivLambda), rkqsLambda);

    if (hDid == h) {
      ++nOk;
    } else {
      ++nBad;
    }

    if ((x - x2) * (x2 - x1) >= 0) {
      ystart = y;
      break;
    }

    h = hNext;
  }

  return minLambda;
}

} // hivModelling

#endif // _hivModelling_CountOdeint_
