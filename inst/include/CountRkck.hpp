#ifndef _hivModelling_CountRkck_
#define _hivModelling_CountRkck_

#include "globals.hpp"
#include "CountModelParameters.hpp"
#include "GetBSpline.hpp"
#include "CountModel.hpp"

namespace hivModelling {

inline void CountRkck(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const double& minYear,
  const double& maxYear,
  double& rkckLambda,
  Rcpp::NumericVector& yOut,
  Rcpp::NumericVector& yErr
) {
  Rcpp::NumericVector ak2(nVar);
  double xtemp = x + a2 * h;
  Rcpp::NumericVector ytemp = y + b21 * h * dydx;
  double lambda2 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda2, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak2
  );

  Rcpp::NumericVector ak3(nVar);
  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  double lambda3 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda3, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak3
  );
  rkckLambda = fmin(lambda2, lambda3);

  Rcpp::NumericVector ak4(nVar);
  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  double lambda4 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda4, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak4
  );
  rkckLambda = fmin(rkckLambda, lambda4);

  Rcpp::NumericVector ak5(nVar);
  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  double lambda5 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda5, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak5
  );
  rkckLambda = fmin(rkckLambda, lambda5);

  Rcpp::NumericVector ak6(nVar);
  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  double lambda6 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda6, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak6
  );
  rkckLambda = fmin(rkckLambda, lambda6);

  yOut = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  yErr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);
}

} // hivModelling

#endif // _hivModelling_CountRkck_
