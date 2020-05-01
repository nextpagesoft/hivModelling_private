#include "odeint.h"
#include "globals.h"
#include "Sign.h"
#include "GetBSpline.h"
#include "rkqs.h"
#include "Models.h"

using namespace Rcpp;

// [[Rcpp::export]]
double odeint_count(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear
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

  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(H1, x2 - x1);

  NumericVector y = clone(ystart);

  double minLambda = VERY_LRG;
  double rkqsLambda = 0;
  double hDid = 0;
  double hNext = 0;
  double rkckLambda = 0;
  NumericVector yOut(nVar);
  NumericVector yErr(nVar);

  double derivLambda;
  NumericVector dydx(nVar);
  NumericVector yscal(nVar);

  for (int nstp = 0; nstp != MAX_STP; ++nstp) {

    derivLambda = GetBSpline(x, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
    CountModel(
      x, y, derivLambda, nVar, qoppa, fInit, alphaP, mu, noStage, delta4Fac, deltaM, tc, dydx
    );

    yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    rkqs_count(
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

// [[Rcpp::export]]
void odeint_time(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const double tmpYear
) {
  const NumericVector& theta = param["Theta"];
  const NumericVector& myKnots = info["MyKnots"];

  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(H1, x2 - x1);

  NumericVector y = clone(ystart);

  List rkqsRes = List::create(
    Named("hDid") = R_NilValue,
    Named("hNext") = R_NilValue
  );

  List rkckRes = List::create(
    Named("YOut") = R_NilValue,
    Named("YErr") = R_NilValue
  );

  NumericVector dydx(nVar);
  NumericVector yscal;

  for (int nstp = 0; nstp != MAX_STP; ++nstp) {

    TimeModel(x, y, param, tmpYear, dydx);

    yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    rkqs_time(
      x, y, dydx, nVar, h, EPS, yscal, param, info, minYear, maxYear, tmpYear, rkqsRes,
      rkckRes
    );

    const double& hDid = rkqsRes["hDid"];

    if (hDid == h) {
      ++nOk;
    } else {
      ++nBad;
    }

    if ((x - x2) * (x2 - x1) >= 0) {
      ystart = y;
      break;
    }

    h = rkqsRes["hNext"];
  }
}

// [[Rcpp::export]]
NumericVector odeintReturn_count(
    NumericVector& ystart,
    const size_t& nVar,
    const double& x1,
    const double& x2,
    const List& param,
    const List& info,
    const double& minYear,
    const double& maxYear
) {
  odeint_count(ystart, nVar, x1, x2, param, info, minYear, maxYear);
  return ystart;
}

// [[Rcpp::export]]
NumericVector odeintReturn_time(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const double tmpYear
) {
  odeint_time(ystart, nVar, x1, x2, param, info, minYear, maxYear, tmpYear);
  return ystart;
}
