#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double GetBSpline_c(
  double time,
  List param,
  List info
) {
  int tmpMinYear = info["TmpMinYear"];
  int tmpMaxYear = info["TmpMaxYear"];
  if (time <= tmpMinYear || time > tmpMaxYear + 1e-7) {
    return 0;
  }

  int kOrder = info["ModelSplOrder"];
  int modelSplineN = info["ModelSplineN"];
  int modelSplOrder = info["ModelSplOrder"];
  NumericVector myKnots = info["MyKnots"];
  NumericVector theta = param["Theta"];

  NumericMatrix bSpline(modelSplineN, modelSplOrder);

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
          bSpline(i, k) +=
            (time - myKnots[i]) * bSpline(i, k - 1) /
              (myKnots[i + k] - myKnots[i]);
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
GetBSpline_c(time, param, info)
*/
