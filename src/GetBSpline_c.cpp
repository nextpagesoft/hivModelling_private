#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double GetBSpline_c(
    double time,
    List param,
    List info
) {
  int kOrder = info["ModelSplOrder"];
  int modelSplineN = info["ModelSplineN"];
  int modelSplOrder = info["ModelSplOrder"];
  NumericVector myKnots = info["MyKnots"];
  NumericVector theta = param["Theta"];

  NumericMatrix bSpline(modelSplineN, modelSplOrder);

  for(int i = 0; i < modelSplineN; ++i) {
    if (time >= myKnots[i] && time < myKnots[i + 1]) {
      bSpline(i, 0) = 1.0;
    }
  }

  for(int k = 1; k < kOrder; ++k) {
     for (int i = 0; i < modelSplineN; ++i) {
       // Rcout << myKnots[i] << " : " << myKnots[i + k + 1] << std::endl;
       if (time >= myKnots[i] && time < myKnots[i + k + 1]) {
         if (myKnots[i + k + 1] != myKnots[i + 1]) {
           bSpline(i, k + 1) +=
             (myKnots[i + k + 1] - time) * bSpline(i + 1, k) /
               (myKnots[i + k + 1] - myKnots[i + 1]);
        }

        if (myKnots[i + k] != myKnots[i]) {
          bSpline(i, k + 1) +=
            (time - myKnots[i]) * bSpline(i, k) /
              (myKnots[i + k] - myKnots[i]);
        }
      }
    }
  }

  Rcout << bSpline << std::endl;

  double val = 0;
  for(int i = 0; i < modelSplineN; ++i) {
    val += theta[i] * bSpline(i, kOrder);
  }

  return val;
}


/*** R
GetBSpline(time, param, info)
*/
