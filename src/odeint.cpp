#include "odeint.h"

using namespace Rcpp;

// [[Rcpp::export]]
List odeint(
  NumericVector ystart,
  int nVar,
  double x1,
  double x2,
  double eps,
  double h1,
  List param,
  List info,
  double minYear,
  double maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear = 0
) {
  NumericVector theta = param["Theta"];
  int kOrder = info["SplineOrder"];
  int modelSplineN = info["ModelSplineN"];
  NumericVector myKnots = info["MyKnots"];

  static const int MAXSTP = 1e+4;
  static const double TINY = 1e-30;
  static const double VERY_LRG = 1e+10;
  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(h1, x2 - x1);

  NumericVector y = ystart;
  double minLambda = VERY_LRG;

  List rkqsRes = List::create(
    Named("X") = R_NilValue,
    Named("Y") = R_NilValue,
    Named("MinLambda") = R_NilValue,
    Named("hDid") = R_NilValue,
    Named("hNext") = R_NilValue
  );

  List rkckRes = List::create(
    Named("YOut") = R_NilValue,
    Named("YErr") = R_NilValue,
    Named("MinLambda") = R_NilValue
  );

  for (int nstp = 0; nstp != MAXSTP; ++nstp) {

    double derivLambda = GetBSpline(x, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
    NumericVector dydx = (*derivsFunc)(x, y, derivLambda, nVar, param, tmpYear);

    NumericVector yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    rkqs(
      x, y, dydx, nVar, h, eps, yscal, param, info, minYear, maxYear, derivsFunc, tmpYear, rkqsRes,
      rkckRes
    );

    x = rkqsRes["X"];
    y = rkqsRes["Y"];
    double rkqsLambda = rkqsRes["MinLambda"];
    double hDid = rkqsRes["hDid"];

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

  List result = List::create(
    Named("YStart") = ystart,
    Named("X") = x,
    Named("NGood") = nOk,
    Named("NBad") = nBad,
    Named("MinLambda") = minLambda
  );

  return result;
}

/*** R
odeint(ystart, nVar, x1, x2, eps, h1, param, info, minYear, maxYear, derivsFuncName, tmpYear)
*/
