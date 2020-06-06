#ifndef _hivModelling_TimeRkck_
#define _hivModelling_TimeRkck_

#include "globals.hpp"
#include "TimeModel.hpp"

namespace hivModelling {

inline void TimeRkck(
  const double& x,
  const Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& h,
  const Rcpp::NumericVector& qoppa,
  const Rcpp::NumericVector& fInit,
  const double& alphaP,
  const double& mu,
  const size_t& noStage,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear,
  Rcpp::List& result
) {
  double xtemp = x + a2 * h;
  Rcpp::NumericVector ytemp = y + b21 * h * dydx;
  Rcpp::NumericVector ak2(nVar);
  TimeModel(xtemp, ytemp, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, tmpYear, ak2);

  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  Rcpp::NumericVector ak3(nVar);
  TimeModel(xtemp, ytemp,  qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, tmpYear, ak3);

  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  Rcpp::NumericVector ak4(nVar);
  TimeModel(xtemp, ytemp, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, tmpYear, ak4);

  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  Rcpp::NumericVector ak5(nVar);
  TimeModel(xtemp, ytemp, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, tmpYear, ak5);

  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  Rcpp::NumericVector ak6(nVar);
  TimeModel(xtemp, ytemp, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, tmpYear, ak6);

  Rcpp::NumericVector yout = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  Rcpp::NumericVector yerr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);

  result["YOut"] = yout;
  result["YErr"] = yerr;
}

} // hivModelling

#endif // _hivModelling_TimeRkck_
