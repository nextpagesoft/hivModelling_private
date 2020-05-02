#ifndef _hivModelling_OdeintCount_
#define _hivModelling_OdeintCount_

#include "RkqsCount.hpp"
#include "Sign.hpp"

namespace hivModelling {

inline double OdeintCount(
  Rcpp::NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const Rcpp::List& param,
  const Rcpp::List& info,
  const double& minYear,
  const double& maxYear
) {
  const Rcpp::NumericVector& myKnots = info["MyKnots"];
  const int& kOrder                  = info["SplineOrder"];
  const int& modelSplineN            = info["ModelSplineN"];
  const Rcpp::NumericVector& theta   = param["Theta"];
  const Rcpp::NumericVector& qoppa   = param["Qoppa"];
  const Rcpp::NumericVector& fInit   = param["FInit"];
  const double& alphaP               = param["AlphaP"];
  const double& mu                   = param["Mu"];
  const size_t& noStage              = param["NoStage"];
  const double& delta4Fac            = param["Delta4Fac"];
  const Rcpp::NumericMatrix& deltaM  = param["DeltaM"];
  const Rcpp::NumericVector& tc      = param["Tc"];

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

    RkqsCount(
      x, y, dydx, nVar, h, EPS, yscal, param, info, minYear, maxYear, rkqsLambda, hDid, hNext,
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

#endif // _hivModelling_OdeintCount_
