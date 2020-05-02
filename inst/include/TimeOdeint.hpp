#ifndef _hivModelling_TimeOdeint_
#define _hivModelling_TimeOdeint_

#include "globals.hpp"
#include "Sign.hpp"
#include "TimeModel.hpp"
#include "TimeRkqs.hpp"

namespace hivModelling {

inline void TimeOdeint(
  Rcpp::NumericVector& ystart,
  const size_t& nVar,
  const double& x1,
  const double& x2,
  const Rcpp::List& param,
  const Rcpp::List& info,
  const double& minYear,
  const double& maxYear,
  const double& tmpYear
) {
  const Rcpp::NumericVector& theta = param["Theta"];
  const Rcpp::NumericVector& myKnots = info["MyKnots"];

  int nBad = 0;
  int nOk = 0;

  double x = x1;
  double h = Sign(H1, x2 - x1);

  Rcpp::NumericVector y = Rcpp::clone(ystart);

  Rcpp::List rkqsRes = Rcpp::List::create(
    Rcpp::Named("hDid") = R_NilValue,
    Rcpp::Named("hNext") = R_NilValue
  );

  Rcpp::List rkckRes = Rcpp::List::create(
    Rcpp::Named("YOut") = R_NilValue,
    Rcpp::Named("YErr") = R_NilValue
  );

  Rcpp::NumericVector dydx(nVar);
  Rcpp::NumericVector yscal;

  for (int nstp = 0; nstp != MAX_STP; ++nstp) {

    TimeModel(x, y, param, tmpYear, dydx);

    yscal = abs(y) + abs(dydx * h) + TINY;

    if ((x + h - x2) * (x + h - x1) > 0) {
      h = x2 - x;
    }

    TimeRkqs(
      x, y, dydx, nVar, h, EPS, yscal, param, info, minYear, maxYear, tmpYear, rkqsRes,
      rkckRes
    );

    const double& hDid = rkqsRes["hDid"];

    if (hDid == h) {
      ++nOk;
    } else {
      ++nBad;
    }

    if ((x - x2) * (x2 - x1) >= 0) {
      ystart = y;
      break;
    }

    h = rkqsRes["hNext"];
  }
}

} // hivModelling

#endif // _hivModelling_TimeOdeint_
