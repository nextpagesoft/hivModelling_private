// Take a Cash-Karp Runge-Kutta step

#include "rkck.h"
#include "GetBSpline.h"

using namespace Rcpp;

// [[Rcpp::export]]
List rkck(
  double x,
  NumericVector y,
  NumericVector dydx,
  int n,
  double h,
  List param,
  List info,
  int minYear,
  int maxYear,
  DerivsFuncXPtr derivsFunc,
  double tmpYear
) {
  NumericVector theta = param["Theta"];
  int kOrder = info["SplineOrder"];
  int modelSplineN = info["ModelSplineN"];
  NumericVector myKnots = info["MyKnots"];

  static const double a2 = 0.2;
  static const double a3 = 0.3;
  static const double a4 = 0.6;
  static const double a5 = 1.0;
  static const double a6 = 0.875;
  static const double b21 = 0.2;
  static const double b31 = 0.07499999999999999722444;
  static const double b32 = 0.2250000000000000055511;
  static const double b41 = 0.3;
  static const double b42 = -0.9;
  static const double b43 = 1.2;
  static const double b51 = -0.2037037037037036923959;
  static const double b52 = 2.5;
  static const double b53 = -2.592592592592592559697;
  static const double b54 = 1.296296296296296279849;
  static const double b61 = 0.02949580439814814686317;
  static const double b62 = 0.341796875;
  static const double b63 = 0.04159432870370370627366;
  static const double b64 = 0.4003454137731481399243;
  static const double b65 = 0.061767578125;
  static const double c1 = 0.09788359788359787816425;
  static const double c3 = 0.4025764895330112835836;
  static const double c4 = 0.2104377104377104512611;
  static const double c6 = 0.289102202145680386991;
  static const double dc5 = -0.01932198660714285615159;
  static const double dc1 = -0.004293774801587310618878;
  static const double dc3 = 0.01866858609385785294776;
  static const double dc4 = -0.03415502683080806622939;
  static const double dc6 = 0.03910220214568038699099;

  double xtemp = x + a2 * h;
  NumericVector ytemp = y + b21 * h * dydx;
  double lambda2 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak2 = (*derivsFunc)(xtemp, ytemp, lambda2, n, param, tmpYear);

  xtemp = x + a3 * h;
  ytemp = y + h * (b31 * dydx + b32 * ak2);
  double lambda3 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak3 = (*derivsFunc)(xtemp, ytemp, lambda3, n, param, tmpYear);

  xtemp = x + a4 * h;
  ytemp = y + h * (b41 * dydx + b42 * ak2 + b43 * ak3);
  double lambda4 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak4 = (*derivsFunc)(xtemp, ytemp, lambda4, n, param, tmpYear);

  xtemp = x + a5 * h;
  ytemp = y + h * (b51 * dydx + b52 * ak2 + b53 * ak3 + b54 * ak4);
  double lambda5 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak5 = (*derivsFunc)(xtemp, ytemp, lambda5, n, param, tmpYear);

  xtemp = x + a6 * h;
  ytemp = y + h * (b61 * dydx + b62 * ak2 + b63 * ak3 + b64 * ak4 + b65 * ak5);
  double lambda6 = GetBSpline(xtemp, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
  NumericVector ak6 = (*derivsFunc)(xtemp, ytemp, lambda6, n, param, tmpYear);

  NumericVector yout = y + h * (c1 * dydx + c3 * ak3 + c4 * ak4 + c6 * ak6);
  NumericVector yerr = h * (dc1 * dydx + dc3 * ak3 + dc4 * ak4 + dc5 * ak5 + dc6 * ak6);

  double minLambda = min(NumericVector::create(lambda2, lambda3, lambda4, lambda5, lambda6));

  List result = List::create(
    Named("YOut") = yout,
    Named("YErr") = yerr,
    Named("MinLambda") = minLambda
  );

  return result;
}

/*** R
rkck(x, y, dydx, n, h, param, info, minYear, maxYear, derivsFunc, tmpYear)
*/
