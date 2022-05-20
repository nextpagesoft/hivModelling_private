#ifndef _hivModelling_ExportCountModelParameters_
#define _hivModelling_ExportCountModelParameters_

#include "CountModelParameters.hpp"

namespace hivModelling {

Rcpp::NumericVector   myKnots;
size_t                kOrder;
size_t                modelSplineN;
Rcpp::NumericVector   theta;
Rcpp::NumericVector   qoppa;
Rcpp::NumericVector   fInit;
double                alphaP;
double                mu;
size_t                noStage;
double                delta4Fac;
Rcpp::NumericMatrix   deltaM;
Rcpp::NumericVector   tc;
size_t                nVar;
size_t                modelMinYear;
size_t                modelMaxYear;
Rcpp::CharacterVector modelResultsColNames;
bool                  usePreCompBSpline;
Rcpp::NumericMatrix   preCompBSpline;

inline void ExportParametersToCpp(
  const Rcpp::List& param,
  const Rcpp::List& info
) {
  // Initialize count model parameters
  myKnots              = info["MyKnots"];
  kOrder               = info["SplineOrder"];
  modelSplineN         = info["ModelSplineN"];
  theta                = param["Theta"];
  qoppa                = param["Qoppa"];
  fInit                = param["FInit"];
  alphaP               = param["AlphaP"];
  mu                   = param["Mu"];
  noStage              = param["NoStage"];
  delta4Fac            = param["Delta4Fac"];
  deltaM               = Rcpp::as<Rcpp::NumericMatrix>(param["DeltaM"]);
  tc                   = param["Tc"];
  nVar                 = param["NoEq"];
  modelMinYear         = info["ModelMinYear"];
  modelMaxYear         = info["ModelMaxYear"];
  modelResultsColNames = param["ModelResultsColNames"];

  if (
    info.containsElementNamed("PreCompBSpline") && !Rf_isNull(info["PreCompBSpline"])
  ) {
    preCompBSpline = Rcpp::as<Rcpp::NumericMatrix>(info["PreCompBSpline"]);
    usePreCompBSpline = true;
  } else {
    usePreCompBSpline = false;
  }
}

} // hivModelling

#endif // _hivModelling_ExportCountModelParameters_
