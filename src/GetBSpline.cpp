#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double GetBSpline(
  double time,
  NumericVector theta,
  int kOrder,
  int modelSplineN,
  NumericVector myKnots,
  double minYear,
  double maxYear
) {
  if (time <= minYear || time > maxYear + 1e-7 || all(theta == 0).is_true()) {
    return 0;
  }

  NumericMatrix bSpline(modelSplineN, kOrder);

  for (int i = 0; i != modelSplineN; ++i) {
    if (myKnots[i] <= time && time < myKnots[i + 1]) {
      bSpline(i, 0) = 1;
    }
  }

  for (int k = 1; k != kOrder; ++k) {
    for (int i = 0; i != modelSplineN; ++i) {
      const int ik = i + k;
      const int ik1 = ik + 1;

      if (myKnots[i] <= time && time < myKnots[ik1]) {
        if (myKnots[ik1] != myKnots[i + 1]) {
          bSpline(i, k) +=
            (myKnots[ik1] - time) * bSpline(i + 1, k - 1) / (myKnots[ik1] - myKnots[i + 1]);
        }

        if (myKnots[ik] != myKnots[i]) {
          bSpline(i, k) += (time - myKnots[i]) * bSpline(i, k - 1) / (myKnots[ik] - myKnots[i]);
        }
      }
    }
  }

  double val = sum(theta * bSpline(_, kOrder - 1));

  return val;
}


/*** R
GetBSpline(time, theta, kOrder, modelSplineN, myKnots, minYear, maxYear)
*/
