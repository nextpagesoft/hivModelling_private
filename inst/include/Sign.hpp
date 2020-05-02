#ifndef _hivModelling_Sign_
#define _hivModelling_Sign_

namespace hivModelling {

inline double Sign(double a, double b) {
  return b >= 0.0 ? fabs(a) : -fabs(a);
}

} // hivModelling

#endif // _hivModelling_Sign_
