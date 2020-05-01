// Takes one "quality-controlled" Runge-Kutta step
#include "rkqs.h"
#include "globals.h"
#include "rkck.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rkqs_count(
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
  double& rkqsLambda,
  double& hDid,
  double& hNext,
  double& rkckLambda,
  NumericVector& yOut,
  NumericVector& yErr
) {
  rkqsLambda = 0;
  rkckLambda = 0;
  double h = htry;

  for (;;) {
    rkck_count(x, y, dydx, nVar, h, param, info, minYear, maxYear, rkckLambda, yOut, yErr);

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

// [[Rcpp::export]]
void rkqs_time(
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
  const double& tmpYear,
  List& rkqsRes,
  List& rkckRes
) {
  NumericVector yerr(nVar);
  NumericVector ytemp(nVar);

  double hNext = 0;
  double hDid = 0;
  double h = htry;

  for (;;) {
    rkck_time(x, y, dydx, nVar, h, param, info, minYear, maxYear, tmpYear, rkckRes);

    const NumericVector& yOut = rkckRes["YOut"];
    const NumericVector& yErr = rkckRes["YErr"];

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
