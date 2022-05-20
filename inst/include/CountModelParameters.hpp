#ifndef _hivModelling_CountModelParameters_
#define _hivModelling_CountModelParameters_

namespace hivModelling {

// Global model parameters
extern Rcpp::NumericVector   myKnots;
extern size_t                kOrder;
extern size_t                modelSplineN;
extern Rcpp::NumericVector   theta;
extern Rcpp::NumericVector   qoppa;
extern Rcpp::NumericVector   fInit;
extern double                alphaP;
extern double                mu;
extern size_t                noStage;
extern double                delta4Fac;
extern Rcpp::NumericMatrix   deltaM;
extern Rcpp::NumericVector   tc;

extern size_t                nVar;
extern size_t                modelMinYear;
extern size_t                modelMaxYear;
extern Rcpp::CharacterVector modelResultsColNames;

extern bool                  usePreCompBSpline;
extern Rcpp::NumericMatrix   preCompBSpline;

} // hivModelling

#endif // _hivModelling_CountModelParameters_
