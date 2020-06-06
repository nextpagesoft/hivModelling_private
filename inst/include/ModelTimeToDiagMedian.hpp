#ifndef _hivModelling_ModelTimeToDiagMedian_
#define _hivModelling_ModelTimeToDiagMedian_

#include "globals.hpp"
#include "TimeOdeint.hpp"
#include "CountModelParameters.hpp"

namespace hivModelling {

inline Rcpp::NumericVector ModelTimeToDiagMedian(
  const double& time
) {
  const size_t nEq = 1 + 2 * noStage;
  const double tmpMinYear = time;
  const double tmpMaxYear = tmpMinYear + 1;

  Rcpp::NumericVector ystart = Rcpp::NumericVector(nEq);
  ystart[0] = 1000;

  double min25 = 100;
  double min50 = 100;
  double min75 = 100;

  double t25 = 0;
  double t50 = 0;
  double t75 = 0;

  int j = 0;
  bool iNowStop = false;

  while (!iNowStop) {
    // Continue calculations until t75 is not updated anymore
    j++;
    const double timeA = (j - 1) * 0.001;
    const double timeB = timeA + 0.001;

    TimeOdeint(ystart, timeA + BIT_SML, timeB - BIT_SML, tmpMinYear, tmpMaxYear, tmpMinYear);

    const double sumYstart = sum(tail(ystart, noStage));

    // The time of each quartile is calculated as the midpoint of the time interval in which the
    // proportion diagnosed is closest to the quartile.
    double dist = fabs(sumYstart / 1000 - 0.25);
    if (dist < min25) {
      min25 = dist;
      t25 = (timeA + timeB) / 2;
    }

    dist = fabs(sumYstart / 1000 - 0.50);
    if (dist < min50) {
      min50 = dist;
      t50 = (timeA + timeB) / 2;
    }

    dist = fabs(sumYstart / 1000 - 0.75);
    if (dist < min75) {
      min75 = dist;
      t75 = (timeA + timeB) / 2;
    } else {
      iNowStop = true;
    }
  }

  return(Rcpp::NumericVector::create(t25, t50, t75));
}

} // hivModelling

#endif // _hivModelling_ModelTimeToDiagMedian_
