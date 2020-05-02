#ifndef _hivModelling_GetBSpline_
#define _hivModelling_GetBSpline_

namespace hivModelling {

inline double GetBSplineCubic(
  const double& time,
  const Rcpp::NumericVector& theta,
  const Rcpp::NumericVector& myKnots,
  const size_t& k
) {
  const double& a = theta[k - 3];
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

inline double GetBSpline(
  const double& time,
  const Rcpp::NumericVector& theta,
  const size_t& kOrder,
  const size_t& modelSplineN,
  const Rcpp::NumericVector& myKnots,
  const double& minYear,
  const double& maxYear
) {
  if (time <= minYear || time > maxYear + 1e-7) {
    return 0;
  }

  size_t k = 0;
  for (k = 0; k != modelSplineN; ++k) {
    if (myKnots[k] <= time && time < myKnots[k + 1]) {
      break;
    }
  }

  if (kOrder == 4) {
    return GetBSplineCubic(time, theta, myKnots, k);
  }

  Rcpp::NumericVector d(kOrder);
  const size_t p = kOrder - 1;
  for (size_t j = 0; j != kOrder; ++j) {
    d[j] = theta[j + k - p];
  }

  for (size_t r = 1; r != kOrder; ++r) {
    for (size_t j = p; j != r - 1; --j) {
      double alpha = (time - myKnots[j + k - p]) / (myKnots[j + k + 1  - r] - myKnots[j + k - p]);
      d[j] = (1 - alpha) * d[j - 1] + alpha * d[j];
    }
  }

  double val = d[p];

  return val;
}

} // namespace

#endif // _hivModelling_GetBSpline_
