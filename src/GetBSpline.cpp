#include <Rcpp.h>

using namespace Rcpp;

double GetBSplineCubic(
  double time,
  NumericVector theta,
  int modelSplineN,
  NumericVector myKnots,
  int k
) {
  double a = theta[k - 3];
  double b = theta[k - 2];
  double c = theta[k - 1];
  double d = theta[k];

  const double term1 = time - myKnots[k];
  const double term2 = time - myKnots[k - 1];

  // r = 1, j = 3
  double alpha = term1 / (myKnots[3 + k] - myKnots[k]);
  d = (1 - alpha) * c + alpha * d;

  // r = 1, j = 2
  alpha = term2 / (myKnots[2 + k] - myKnots[k - 1]);
  c = (1 - alpha) * b + alpha * c;

  // r = 1, j = 1
  alpha = (time - myKnots[k - 2]) / (myKnots[1 + k] - myKnots[k - 2]);
  b = (1 - alpha) * a + alpha * b;

  // r = 2, j = 3
  alpha = term1 / (myKnots[2 + k] - myKnots[k]);
  d = (1 - alpha) * c + alpha * d;

  // r = 2, j = 2
  alpha = term2 / (myKnots[1 + k] - myKnots[k - 1]);
  c = (1 - alpha) * b + alpha * c;

  // r = 3, j = 3
  alpha = term1 / (myKnots[1 + k] - myKnots[k]);
  d = (1 - alpha) * c + alpha * d;

  return d;
}

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
  if (time <= minYear || time > maxYear + 1e-7) {
    return 0;
  }

  int k = 0;
  for (int i = 0; i != modelSplineN; ++i) {
    if (myKnots[i] <= time && time < myKnots[i + 1]) {
      k = i;
      break;
    }
  }

  if (kOrder == 4) {
    return GetBSplineCubic(time, theta, modelSplineN, myKnots, k);
  }

  NumericVector d(kOrder);
  int p = kOrder - 1;
  for (int j = 0; j != kOrder; ++j) {
    d[j] = theta[j + k - p];
  }

  for (int r = 1; r != kOrder; ++r) {
    for (int j = p; j != r - 1; --j) {
      double alpha = (time - myKnots[j + k - p]) / (myKnots[j + k + 1  - r] - myKnots[j + k - p]);
      d[j] = (1 - alpha) * d[j - 1] + alpha * d[j];
    }
  }

  double val = d[p];

  return val;
}

/*** R
GetBSpline(time, theta, kOrder, modelSplineN, myKnots, minYear, maxYear)
*/
