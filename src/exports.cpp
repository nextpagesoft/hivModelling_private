#include "header.h"

// [[Rcpp::export]]
Rcpp::NumericVector GetDelta(
  const double& time,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  const size_t& deadStageIdx
) {
  return hivModelling::GetDelta(time, delta4Fac, deltaM, tc, deadStageIdx);
}

// [[Rcpp::export]]
double GetBSplineCubic(
  const double& time,
  const Rcpp::NumericVector& theta,
  const Rcpp::NumericVector& myKnots,
  const size_t& k
)  {
  return hivModelling::GetBSplineCubic(time, theta, myKnots, k);
}

// [[Rcpp::export]]
double GetBSpline(
  const double& time,
  const Rcpp::NumericVector& theta,
  const size_t& kOrder,
  const size_t& modelSplineN,
  const Rcpp::NumericVector& myKnots,
  const double& minYear,
  const double& maxYear
)  {
  return hivModelling::GetBSpline(time, theta, kOrder, modelSplineN, myKnots, minYear, maxYear);
}

// [[Rcpp::export]]
void Swap1D(
  Rcpp::NumericVector& y,
  const int a,
  const int b
) {
  hivModelling::Swap1D(y, a, b);
}

// [[Rcpp::export]]
void Swap2D(
  Rcpp::NumericMatrix& y,
  const int a1,
  const int a2,
  const int b1,
  const int b2
) {
  hivModelling::Swap2D(y, a1, a2, b1, b2);
}

// [[Rcpp::export]]
Rcpp::List OdeintCountLoop(
    const Rcpp::NumericVector& modelYears,
    const Rcpp::List& param,
    const Rcpp::List& info
) {
  return hivModelling::OdeintCountLoop(modelYears, param, info);
}

// [[Rcpp::export]]
Rcpp::NumericVector OdeintCountReturn(
    Rcpp::NumericVector& ystart,
    const size_t& nVar,
    const double& x1,
    const double& x2,
    const Rcpp::List& param,
    const Rcpp::List& info,
    const double& minYear,
    const double& maxYear
) {
  hivModelling::OdeintCount(ystart, nVar, x1, x2, param, info, minYear, maxYear);
  return ystart;
}

// [[Rcpp::export]]
Rcpp::NumericVector OdeintTimeReturn(
    Rcpp::NumericVector& ystart,
    const size_t& nVar,
    const double& x1,
    const double& x2,
    const Rcpp::List& param,
    const Rcpp::List& info,
    const double& minYear,
    const double& maxYear,
    const double& tmpYear
) {
  hivModelling::OdeintTime(ystart, nVar, x1, x2, param, info, minYear, maxYear, tmpYear);
  return ystart;
}

// [[Rcpp::export]]
Rcpp::NumericVector FitLLNegBin(
  const Rcpp::NumericVector& y_m,
  const Rcpp::NumericVector& y_d,
  const double& r
) {
  return hivModelling::FitLLNegBin(y_m, y_d, r);
}

// [[Rcpp::export]]
Rcpp::NumericVector FitLLPoisson(
    const Rcpp::NumericVector& y_m,
    const Rcpp::NumericVector& y_d
) {
  return hivModelling::FitLLPoisson(y_m, y_d);
}

// [[Rcpp::export]]
double Zbrent(
  const Rcpp::Function& func,
  double x1,
  double x2,
  double tol,
  const Rcpp::List& extraArgs
) {
  return hivModelling::Zbrent(func, x1, x2, tol, extraArgs);
}
