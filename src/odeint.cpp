#include "odeint.h"
#include "globals.h"
#include "Sign.h"
#include "GetBSpline.h"
#include "rkqs.h"

using namespace Rcpp;

// [[Rcpp::export]]
double odeint(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double tmpYear = 0
) {
  const NumericVector& theta = param["Theta"];
  const NumericVector& myKnots = info["MyKnots"];
  const int& kOrder = info["SplineOrder"];
  const int& modelSplineN = info["ModelSplineN"];

  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(H1, x2 - x1);

  NumericVector y = clone(ystart);
  double minLambda = VERY_LRG;

  List rkqsRes = List::create(
    Named("MinLambda") = R_NilValue,
    Named("hDid") = R_NilValue,
    Named("hNext") = R_NilValue
  );

  List rkckRes = List::create(
    Named("YOut") = R_NilValue,
    Named("YErr") = R_NilValue,
    Named("MinLambda") = R_NilValue
  );

  double derivLambda;
  NumericVector dydx(nVar);
  NumericVector yscal;

  for (int nstp = 0; nstp != MAX_STP; ++nstp) {

    derivLambda = GetBSpline(x, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
    (*derivsFunc)(x, y, derivLambda, nVar, param, tmpYear, dydx);

    yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    rkqs(
      x, y, dydx, nVar, h, EPS, yscal, param, info, minYear, maxYear, derivsFunc, tmpYear, rkqsRes,
      rkckRes
    );

    const double& rkqsLambda = rkqsRes["MinLambda"];
    const double& hDid = rkqsRes["hDid"];

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

    h = rkqsRes["hNext"];
  }

  return minLambda;
}

// [[Rcpp::export]]
NumericVector odeintReturn(
  NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const List& param,
  const List& info,
  const double& minYear,
  const double& maxYear,
  const DerivsFuncXPtr& derivsFunc,
  const double tmpYear = 0
) {
  odeint(ystart, nVar, x1, x2, param, info, minYear, maxYear, derivsFunc, tmpYear);

  return ystart;
}
