#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector GetDelta_c(
  double time,
  List param
) {
  int noStage = param["NoStage"];
  int noStageTot = param["NoStageTot"];
  double deltaAIDS = param["DeltaAIDS"];
  double delta4Fac = param["Delta4Fac"];
  NumericMatrix deltaM = param["DeltaM"];
  IntegerVector tc = param["Tc"];

  NumericVector delta(noStageTot);

  // First determine the time interval
  int iTime = 0;
  double dist = 10000;
  while (dist >= 0) {
    dist = time - tc[iTime];
    iTime++;
  }
  iTime--;

  for (int i = 0; i < noStage - 1; ++i) {
    delta[i] = 0;
    for (int j = 0; j < iTime - 1; ++j) {
      delta[i] += deltaM(i, j);
    }
    delta[i] += deltaM(i, iTime - 1) * (time - tc[iTime - 1]) / (tc[iTime] - tc[iTime - 1]);
  }

  // AIDS stage : diagnosis rate is always constant
  delta[noStage - 1] = deltaM(noStage - 1, iTime - 1);

  // Check that the diagnosis rate is as expected
  if (delta[noStage - 1] != deltaAIDS) {
    stop("AIDS rate not AIDS rate in GetDelta_c");
  }

  // Add a constant to the diagnosis rate in the penultimate CD4 category
  if (iTime >= 1) {
    delta[noStage - 2] = delta[noStage - 2] + delta4Fac;
  }

  // Diagnosis rate when dead
  delta[noStageTot] = 0;

  return delta;
}

/*** R
GetDelta_c(time, param)
*/
