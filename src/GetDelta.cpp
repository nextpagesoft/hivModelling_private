#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
void Test(
  NumericVector& x
) {
  x[1] = 23;
}

// [[Rcpp::export]]
size_t GetTimeInterval_std(
  const double& time,
  const NumericVector& timeIntervals
) {
  NumericVector::const_iterator pos;

  pos = std::upper_bound(timeIntervals.begin(), timeIntervals.end(), time);
  return std::distance(timeIntervals.begin(), pos);
}

// [[Rcpp::export]]
size_t GetTimeInterval_std_lower(
    const double& time,
    const NumericVector& timeIntervals
) {
  NumericVector::const_iterator pos;

  pos = std::lower_bound(timeIntervals.begin(), timeIntervals.end(), time);
  return std::distance(timeIntervals.begin(), pos);
}

// [[Rcpp::export]]
size_t GetTimeInterval_original(
  const double& time,
  const NumericVector& tc
) {
  size_t iTime = 0;
  double dist = 10000;
  while (dist >= 0) {
    dist = time - tc[iTime];
    iTime++;
  }
  iTime--;
  iTime--;
  return iTime;
}

// [[Rcpp::export]]
size_t GetTimeInterval(
  const double& x,
  const NumericVector& tc
) {
  const size_t n = tc.size();
  size_t i = 0;
  for (; i != n; ++i) {
    if (x < tc[i]) {
      break;
    }
  }
  i--;

  return i;
}

// [[Rcpp::export]]
NumericVector GetDelta(
  const double& time,
  const List& param
) {
  const double& delta4Fac      = param["Delta4Fac"];
  const NumericMatrix& deltaM = param["DeltaM"];
  const NumericVector& tc     = param["Tc"];
  const size_t deadStageIdx   = param["NoStage"];
  const size_t stageCount     = deadStageIdx - 1;

  const size_t timeInterval = GetTimeInterval(time, tc);
  const double ratio = (time - tc[timeInterval]) / (tc[timeInterval + 1] - tc[timeInterval]);
  NumericVector delta(deadStageIdx + 1);

  // CD4 stages
  for (size_t i = 0; i != stageCount; ++i) {
    for (size_t j = 0; j != timeInterval; ++j) {
      delta[i] += deltaM(i, j);
    }
    delta[i] += deltaM(i, timeInterval) * ratio;
  }

  // AIDS stage: diagnosis rate is always constant
  delta[stageCount] = deltaM(stageCount, timeInterval);

  // Add a constant to the diagnosis rate in the penultimate CD4 category
  if (timeInterval > 0) {
    delta[stageCount - 1] += delta4Fac;
  }

  return delta;
}
