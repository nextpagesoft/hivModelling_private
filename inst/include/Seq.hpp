#ifndef _hivModelling_Seq_
#define _hivModelling_Seq_

namespace hivModelling {

inline Rcpp::IntegerVector Seq(
  const int& start,
  const int& end
) {
  Rcpp::IntegerVector seqVec = Rcpp::seq(start, end);

  return seqVec;
}

} // namespace

#endif // _hivModelling_Seq_
