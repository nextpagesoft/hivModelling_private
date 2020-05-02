#ifndef _hivModelling_GetDelta_
#define _hivModelling_GetDelta_

namespace hivModelling {

inline size_t GetTimeInterval_std(
  const double& time,
  const Rcpp::NumericVector& timeIntervals
) {
  Rcpp::NumericVector::const_iterator pos;

  pos = std::upper_bound(timeIntervals.begin(), timeIntervals.end(), time);
  return std::distance(timeIntervals.begin(), pos) - 1;
}

inline size_t GetTimeInterval(
  const double& x,
  const Rcpp::NumericVector& tc
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

inline Rcpp::NumericVector GetDelta(
  const double& time,
  const double& delta4Fac,
  const Rcpp::NumericMatrix& deltaM,
  const Rcpp::NumericVector& tc,
  const size_t& deadStageIdx
) {
  const size_t stageCount = deadStageIdx - 1;

  const size_t timeInterval = GetTimeInterval(time, tc);
  const double ratio = (time - tc[timeInterval]) / (tc[timeInterval + 1] - tc[timeInterval]);
  Rcpp::NumericVector delta(deadStageIdx + 1);

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

}

#endif // _hivModelling_GetDelta_
