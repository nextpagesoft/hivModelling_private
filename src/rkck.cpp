// Take a Cash-Karp Runge-Kutta step
#include "rkck.h"
#include "globals.h"
#include "GetBSpline.h"
#include "Models.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rkck_count(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  double& rkckLambda,
  NumericVector& yOut,
  NumericVector& yErr
) {
  const NumericVector& myKnots = info["MyKnots"];
  const int& kOrder            = info["SplineOrder"];
  const int& modelSplineN      = info["ModelSplineN"];
  const NumericVector& theta   = param["Theta"];
  const NumericVector& qoppa   = param["Qoppa"];
  const NumericVector& fInit   = param["FInit"];
  const double& alphaP         = param["AlphaP"];
  const double& mu             = param["Mu"];
  const size_t& noStage        = param["NoStage"];
  const double& delta4Fac      = param["Delta4Fac"];
  const NumericMatrix& deltaM  = param["DeltaM"];
  const NumericVector& tc      = param["Tc"];

  NumericVector ak2(nVar);
  double xtemp = x + a2 * h;
  NumericVector ytemp = y + b21 * h * dydx;
  double lambda2 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda2, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak2
  );

  NumericVector ak3(nVar);
  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  double lambda3 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda3, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak3
  );
  rkckLambda = fmin(lambda2, lambda3);

  NumericVector ak4(nVar);
  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  double lambda4 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda4, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak4
  );
  rkckLambda = fmin(rkckLambda, lambda4);

  NumericVector ak5(nVar);
  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  double lambda5 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  CountModel(
    xtemp, ytemp, lambda5, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, ak5
  );
  rkckLambda = fmin(rkckLambda, lambda5);

  NumericVector ak6(nVar);
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

// [[Rcpp::export]]
void rkck_time(
  const double& x,
  const NumericVector& y,
  const NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear,
  List& result
) {
  double xtemp = x + a2 * h;
  NumericVector ytemp = y + b21 * h * dydx;
  NumericVector ak2(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak2);

  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  NumericVector ak3(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak3);

  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  NumericVector ak4(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak4);

  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  NumericVector ak5(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak5);

  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  NumericVector ak6(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak6);

  NumericVector yout = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  NumericVector yerr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);

  result["YOut"] = yout;
  result["YErr"] = yerr;
}
