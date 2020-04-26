// Take a Cash-Karp Runge-Kutta step
#include "rkck.h"
#include "globals.h"
#include "GetBSpline.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rkck(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double& tmpYear,
  List& result
) {
  const NumericVector& theta = param["Theta"];
  const NumericVector& myKnots = info["MyKnots"];
  const int& kOrder = info["SplineOrder"];
  const int& modelSplineN = info["ModelSplineN"];

  double xtemp = x + a2 * h;
  NumericVector ytemp = y + b21 * h * dydx;
  double lambda2 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak2(nVar);
  (*derivsFunc)(xtemp, ytemp, lambda2, nVar, param, tmpYear, ak2);

  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  double lambda3 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  double minLambda = fmin(lambda2, lambda3);
  NumericVector ak3(nVar);
  (*derivsFunc)(xtemp, ytemp, lambda3, nVar, param, tmpYear, ak3);

  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  double lambda4 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  minLambda = fmin(minLambda, lambda4);
  NumericVector ak4(nVar);
  (*derivsFunc)(xtemp, ytemp, lambda4, nVar, param, tmpYear, ak4);

  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  double lambda5 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  minLambda = fmin(minLambda, lambda5);
  NumericVector ak5(nVar);
  (*derivsFunc)(xtemp, ytemp, lambda5, nVar, param, tmpYear, ak5);

  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  double lambda6 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  minLambda = fmin(minLambda, lambda6);
  NumericVector ak6(nVar);
  (*derivsFunc)(xtemp, ytemp, lambda6, nVar, param, tmpYear, ak6);

  NumericVector yout = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  NumericVector yerr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);

  result["YOut"] = yout;
  result["YErr"] = yerr;
  result["MinLambda"] = minLambda;
}
