amoeba <- function(
  p,
  y,
  ndim,
  ftol,
  probSurv1996,
  param,
  info,
  data
) {

  DisplayMessage <- function(rtol, nfunk, ytry) {
    strs <- formatC(c(rtol, nfunk, ytry), width = 10, preserve.width = 'common')
    message(sprintf('rtol = %s | nfunk = %s | ytry = %s', strs[1], strs[2], strs[3]))
  }

  Swap1D <- function(y, a, b) {
    s <- y[a]
    y[a] <- y[b]
    y[b] <- s
    return(y)
  }

  Swap2D <- function(y, a1, a2, b1, b2) {
    s <- y[a1, a2]
    y[a1, a2] <- y[b1, b2]
    y[b1, b2] <- s
    return(y)
  }

  AmoebaTry <- function(
    p, y, psum, ndim, ihi, fac,
    probSurv1996,
    param,
    info,
    data)
  {
    fac1 <- (1.0 - fac) / ndim
    fac2 <- fac1 - fac

    ptry <- psum * fac1 - p[ihi, ] * fac2

    res <- FitLLTotal(ptry,
                      probSurv1996,
                      param,
                      info,
                      data)
    ytry <- res$LLTotal

    if (ytry < y[ihi]) {
      y[ihi] <- ytry
      psum <- psum + ptry - p[ihi, ]
      p[ihi, ] <- ptry
    }

    return(list(
      Ytry = ytry,
      Y = y,
      Psum = psum,
      P = p
    ))
  }

  # Code -----------------------------------------------------------------------

  NMAX <- 50000
  mpts <- ndim + 1
  nfunk <- 0
  psum <- colSums(p)
  while (
    nfunk < NMAX
  ) {
    inhi <- 0
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

    if (rtol < ftol) {
      y <- Swap1D(y, 1, ilo)
      for (i in seq_len(ndim)) {
        p <- Swap2D(p, 1, i, ilo, i)
      }

      break
    }

    if (nfunk >= NMAX) {
      message('NMAX exceeded')
    }

    nfunk <- nfunk + 2

    # Begin a new iteration. First extrapolate by a factor -1 through the face of the
    # simplex across from the high points, i.e., reflect the simplex from the high point.
    res <- AmoebaTry(p, y, psum, ndim, ihi, fac = -1,
                     probSurv1996,
                     param,
                     info,
                     data)
    ytry <- res$Ytry
    y <- res$Y
    psum <- res$Psum
    p <- res$P

    if (
      ytry <= y[ilo]
    ) {
      # Gives a result better than the best point, so try an additional extrapolation by a factor 2.
      res <- AmoebaTry(p, y, psum, ndim, ihi, fac = 2.0,
                       probSurv1996,
                       param,
                       info,
                       data)
      ytry <- res$Ytry
      y <- res$Y
      psum <- res$Psum
      p <- res$P

    } else if (
      ytry >= y[inhi]
    ) {
      ysave <- y[ihi]
      res <- AmoebaTry(p, y, psum, ndim, ihi, fac = 0.5,
                       probSurv1996,
                       param,
                       info,
                       data)
      ytry <- res$Ytry
      y <- res$Y
      psum <- res$Psum
      p <- res$P

      if (
        ytry >= ysave
      ) {
        # Can't seem to get rid of that high point. Better contract around the lowest (best) point.
        # i <- 1
				for (i in seq_len(mpts)) {
					if (i != ilo) {
					  psum <- 0.5 * (p[i, ] + p[ilo, ])
					  p[i, ] <- psum

						res <- FitLLTotal(psum,
						                  probSurv1996,
						                  param,
						                  info,
						                  data)
						y[i] <- res$LLTotal
					}
				}
				# Keep track of function evaluations
				nfunk <- nfunk + ndim

				# Recompute psum
				psum <- colSums(p)
			}
		} else {
			# Correct the evaluation count
		  nfunk <- nfunk - 1
		}

    DisplayMessage(rtol, nfunk, ytry)
  }

  finalResults <- FitLLTotal(p, probSurv1996, param, info, data)

  return(list(
    P = p[1,],
    AmoebaP = p,
    LLTotal = finalResults$LLTotal,
    ModelResults = finalResults$ModelResults
  ))
}
