#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double GetBSpline(
  double time,
  List param,
  List info,
  double minYear,
  double maxYear
) {
  if (time <= minYear || time > maxYear + 1e-7) {
    return 0;
  }

  int kOrder = info["SplineOrder"];
  int modelSplineN = info["ModelSplineN"];
  NumericVector myKnots = info["MyKnots"];
  NumericVector theta = param["Theta"];

  NumericMatrix bSpline(modelSplineN, kOrder);

  for (int i = 0; i < modelSplineN; ++i) {
    if (time >= myKnots[i] && time < myKnots[i + 1]) {
      bSpline(i, 0) = 1.0;
    }
  }

  for (int k = 1; k < kOrder; ++k) {
    for (int i = 0; i < modelSplineN; ++i) {
      int ik1 = i + k + 1;

      if (time >= myKnots[i] && time < myKnots[ik1]) {
        if (myKnots[ik1] != myKnots[i + 1]) {
          bSpline(i, k) +=
            (myKnots[ik1] - time) * bSpline(i + 1, k - 1) / (myKnots[ik1] - myKnots[i + 1]);
        }

        if (myKnots[i + k] != myKnots[i]) {
          bSpline(i, k) +=
            (time - myKnots[i]) * bSpline(i, k - 1) / (myKnots[i + k] - myKnots[i]);
        }
      }
    }
  }

  double val = sum(theta * bSpline(_, kOrder - 1));

  return val;
}

/*** R
GetBSpline(time, param, info, minYear, maxYear)
*/
