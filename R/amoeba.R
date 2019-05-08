amoeba <- function(
  p,
  y,
  ndim,
  ftol,
  funk,
  deltaP,
  deltaM,
  theta,
  thetaP,
  noThetaFix,
  noDelta,
  modelSplineN,
  modelNoYears,
  modelYears,
  splineType,
  maxIncCorr,
  noEq,
  noStage,
  probSurv1996,
  model,
  param,
  info,
  data,
  extraResults
) {

  swap1D <- function(y, a, b) {
    s <- y[a]
    y[a] <- y[b]
    y[b] <- s
    return(y)
  }

  swap2D <- function(y, a1, a2, b1, b2) {
    s <- y[a1, a2]
    y[a1, a2] <- y[b1, b2]
    y[b1, b2] <- s
    return(y)
  }

  getPsum <- function(p) {
    psum <- colSums(p)
  }

  amotry <- function(p, y, psum, ndim, funk, ihi, fac,
                     deltaP,
                     deltaM,
                     theta,
                     thetaP,
                     noThetaFix,
                     noDelta,
                     modelSplineN,
                     modelNoYears,
                     modelYears,
                     splineType,
                     maxIncCorr,
                     noEq,
                     noStage,
                     probSurv1996,
                     model,
                     param,
                     info,
                     data,
                     extraResults)
  {
    fac1 <- (1.0 - fac) / ndim
    fac2 <- fac1 - fac

    ptry <- psum * fac1 - p[ihi, ] * fac2

    ytry <- funk(ptry,
                 deltaP,
                 deltaM,
                 theta,
                 thetaP,
                 noThetaFix,
                 noDelta,
                 modelSplineN,
                 modelNoYears,
                 modelYears,
                 splineType,
                 maxIncCorr,
                 noEq,
                 noStage,
                 probSurv1996,
                 model,
                 param,
                 info,
                 data,
                 extraResults)
    if (ytry < y[ihi]) {
      y[ihi] <- ytry
      psum <- psum + ptry - p[ihi, ]
      p[ihi, ] <- ptry
      # for (j in seq_len(ndim)) {
      #   psum[j] <- psum[j] + ptry[j] - p[ihi][j]
      #   p[ihi, j] <- ptry[j]
      # }
    }

    return(ytry)
  }

  NMAX <- 50000
  mpts <- ndim + 1
  nfunk <- 0
  psum <- getPsum(p)
  while (
    nfunk < NMAX
  ) {
    ilo <- 1
    if (y[1] > y[2]) {
      inhi <- 2
      ihi <- 1
    } else {
      inhi <- 1
      ihi <- 2
    }

    # i <- 1
    for (i in seq_len(mpts)) {
      if (y[i] <= y[ilo]) {
        ilo <- i
      }

      if (y[i] > y[ihi]) {
        inhi <- ihi
        ihi <- i
      } else if (y[i] > y[inhi] && i != ihi) {
        inhi <- i
      }
    }

    rtol <- 2.0 * abs(y[ihi] - y[ilo]) / (abs(y[ihi]) + abs(y[ilo]))

    print(rtol)
    if (rtol < ftol) {
      y <- swap1D(y, 1, ilo)
      for (i in seq_len(ndim)) {
        p <- swap2D(p, 1, i, ilo, i)
      }

      print('here')
      break
    }

    if (nfunk >= NMAX) {
      message('NMAX exceeded')
    }

    nfunk <- nfunk + 2

    # Begin a new iteration. First extrapolate by a factor -1 through the face of the
    # simplex across from the high points, i.e., reflect the simplex from the high point.
    ytry <- amotry(p, y, psum, ndim, funk, ihi, fac = -1.0,
                   deltaP,
                   deltaM,
                   theta,
                   thetaP,
                   noThetaFix,
                   noDelta,
                   modelSplineN,
                   modelNoYears,
                   modelYears,
                   splineType,
                   maxIncCorr,
                   noEq,
                   noStage,
                   probSurv1996,
                   model,
                   param,
                   info,
                   data,
                   extraResults)

    if (ytry <= y[ilo]) {
      # Gives a result better than the best point, so try an additional extrapolation by a factor 2.
      ytry <- amotry(p, y, psum, ndim, funk, ihi, 2.0,
                     deltaP,
                     deltaM,
                     theta,
                     thetaP,
                     noThetaFix,
                     noDelta,
                     modelSplineN,
                     modelNoYears,
                     modelYears,
                     splineType,
                     maxIncCorr,
                     noEq,
                     noStage,
                     probSurv1996,
                     model,
                     param,
                     info,
                     data,
                     extraResults)
    } else if (
      ytry >= y[inhi]
    ) {
      ysave <- y[ihi]
      ytry <- amotry(p, y, psum, ndim, funk, ihi, 0.5,
                     deltaP,
                     deltaM,
                     theta,
                     thetaP,
                     noThetaFix,
                     noDelta,
                     modelSplineN,
                     modelNoYears,
                     modelYears,
                     splineType,
                     maxIncCorr,
                     noEq,
                     noStage,
                     probSurv1996,
                     model,
                     param,
                     info,
                     data,
                     extraResults)
      if (ytry >= ysave) {
        # Can't seem to get rid of that high point. Better contract around the lowest (best) point.
        # i <- 10
				for (i in seq_len(mpts)) {
					if (i != ilo) {
					  psum <- 0.5*(p[i, ] + p[ilo, ])
					  p[i, ] <- psum
#             for (j in seq_len(ndim)) {
# 						  psum[j] <- 0.5*(p[i, j] + p[ilo, j])
# 						  p[i, j] <- psum[j]
# 						}
						y[i] <- funk(psum,
						             deltaP,
						             deltaM,
						             theta,
						             thetaP,
						             noThetaFix,
						             noDelta,
						             modelSplineN,
						             modelNoYears,
						             modelYears,
						             splineType,
						             maxIncCorr,
						             noEq,
						             noStage,
						             probSurv1996,
						             model,
						             param,
						             info,
						             data,
						             extraResults)
					}
				}
				# Keep track of function evaluations
				nfunk <- nfunk + ndim

				# Recompute psum
				psum <- getPsum(p)
			}
		} else {
			# Correct the evaluation count
		  nfunk <- nfunk - 1
		}
    # Go back for the test of doneness and the next iteration.
  }

  return(p)
}
