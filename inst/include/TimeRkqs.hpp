#ifndef _hivModelling_TimeRkqs_
#define _hivModelling_TimeRkqs_

#include "globals.hpp"
#include "TimeRkck.hpp"

namespace hivModelling {

inline void TimeRkqs(
  double& x,
  Rcpp::NumericVector& y,
  const Rcpp::NumericVector& dydx,
  const size_t& nVar,
  const double& htry,
  const double& eps,
  const Rcpp::NumericVector& yscal,
  const Rcpp::List& param,
  const Rcpp::List& info,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear,
  Rcpp::List& rkqsRes,
  Rcpp::List& rkckRes
) {
  Rcpp::NumericVector yerr(nVar);
  Rcpp::NumericVector ytemp(nVar);

  double hNext = 0;
  double hDid = 0;
  double h = htry;

  for (;;) {
    TimeRkck(x, y, dydx, nVar, h, param, info, minYear, maxYear, tmpYear, rkckRes);

    const Rcpp::NumericVector& yOut = rkckRes["YOut"];
    const Rcpp::NumericVector& yErr = rkckRes["YErr"];

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

  rkqsRes["hDid"] = hDid;
  rkqsRes["hNext"] = hNext;
}

} // hivModelling

#endif // _hivModelling_TimeRkqs_
