#ifndef _hivModelling_header_
#define _hivModelling_header_

#include <Rcpp.h>

#include "CountOdeintLoop.hpp"
#include "CountOdeint.hpp"
#include "CountRkqs.hpp"
#include "CountRkck.hpp"
#include "CountModel.hpp"
#include "SetCountModelParameters.hpp"

#include "TimeOdeint.hpp"
#include "TimeRkqs.hpp"
#include "TimeRkck.hpp"
#include "TimeModel.hpp"
#include "ModelTimeToDiagMedian.hpp"

#include "GetDelta.hpp"
#include "GetBSpline.hpp"
#include "Sign.hpp"
#include "Zbrent.hpp"
#include "Swap.hpp"
#include "FitLLNegBin.hpp"
#include "FitLLPoisson.hpp"
#include "Seq.hpp"

#endif // _hivModelling_header_
