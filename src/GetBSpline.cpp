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
       if (time >= myKnots[i] && time < myKnots[i + k + 1]) {
         if (myKnots[i + k + 1] != myKnots[i + 1]) {
           bSpline(i, k) +=
             (myKnots[i + k + 1] - time) * bSpline(i + 1, k - 1) /
               (myKnots[i + k + 1] - myKnots[i + 1]);
        }

        if (myKnots[i + k] != myKnots[i]) {
          bSpline(i, k) += (time - myKnots[i]) * bSpline(i, k - 1) / (myKnots[i + k] - myKnots[i]);
        }
      }
    }
  }

  double val = 0;
  for (int i = 0; i < modelSplineN; ++i) {
    val += theta[i] * bSpline(i, kOrder - 1);
  }

  return val;
}

/*** R
GetBSpline(time, param, info, minYear, maxYear)
*/
