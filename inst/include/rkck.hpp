#ifndef _hivModelling_rkck_
#define _hivModelling_rkck_

#include "GetBSpline.hpp"
#include "Models.hpp"

namespace hivModelling {

// Cash-Karp constants
constexpr double a2 = 0.2;
constexpr double a3 = 0.3;
constexpr double a4 = 0.6;
constexpr double a5 = 1.0;
constexpr double a6 = 0.875;
constexpr double b21 = 0.2;
constexpr double b31 = 0.07499999999999999722444;
constexpr double b32 = 0.2250000000000000055511;
constexpr double b41 = 0.3;
constexpr double b42 = -0.9;
constexpr double b43 = 1.2;
constexpr double b51 = -0.2037037037037036923959;
constexpr double b52 = 2.5;
constexpr double b53 = -2.592592592592592559697;
constexpr double b54 = 1.296296296296296279849;
constexpr double b61 = 0.02949580439814814686317;
constexpr double b62 = 0.341796875;
constexpr double b63 = 0.04159432870370370627366;
constexpr double b64 = 0.4003454137731481399243;
constexpr double b65 = 0.061767578125;
constexpr double c1 = 0.09788359788359787816425;
constexpr double c3 = 0.4025764895330112835836;
constexpr double c4 = 0.2104377104377104512611;
constexpr double c6 = 0.289102202145680386991;
constexpr double dc5 = -0.01932198660714285615159;
constexpr double dc1 = -0.004293774801587310618878;
constexpr double dc3 = 0.01866858609385785294776;
constexpr double dc4 = -0.03415502683080806622939;
constexpr double dc6 = 0.03910220214568038699099;

inline void rkck_count(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const Rcpp::List& param,
  const Rcpp::List& info,
  const double& minYear,
  const double& maxYear,
  double& rkckLambda,
  Rcpp::NumericVector& yOut,
  Rcpp::NumericVector& yErr
) {
  const Rcpp::NumericVector& myKnots = info["MyKnots"];
  const int& kOrder            = info["SplineOrder"];
  const int& modelSplineN      = info["ModelSplineN"];
  const Rcpp::NumericVector& theta   = param["Theta"];
  const Rcpp::NumericVector& qoppa   = param["Qoppa"];
  const Rcpp::NumericVector& fInit   = param["FInit"];
  const double& alphaP         = param["AlphaP"];
  const double& mu             = param["Mu"];
  const size_t& noStage        = param["NoStage"];
  const double& delta4Fac      = param["Delta4Fac"];
  const Rcpp::NumericMatrix& deltaM  = param["DeltaM"];
  const Rcpp::NumericVector& tc      = param["Tc"];

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

inline void rkck_time(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const Rcpp::List& param,
  const Rcpp::List& info,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear,
  Rcpp::List& result
) {
  double xtemp = x + a2 * h;
  Rcpp::NumericVector ytemp = y + b21 * h * dydx;
  Rcpp::NumericVector ak2(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak2);

  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  Rcpp::NumericVector ak3(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak3);

  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  Rcpp::NumericVector ak4(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak4);

  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  Rcpp::NumericVector ak5(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak5);

  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  Rcpp::NumericVector ak6(nVar);
  TimeModel(xtemp, ytemp, param, tmpYear, ak6);

  Rcpp::NumericVector yout = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  Rcpp::NumericVector yerr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);

  result["YOut"] = yout;
  result["YErr"] = yerr;
}

} // hivModelling

#endif // _hivModelling_rkck_
