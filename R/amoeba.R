amoeba <- function(
  p,
  y,
  ndim,
  ftol,
  probSurv1996,
  param,
  info,
  data,
  verbose = FALSE
) {
  if (verbose) {
    DisplayMessage <- function(rtol, nfunk, ytry, fac, y9) {
      strs <- formatC(
        c(rtol, nfunk, ytry, fac, y9),
        width = 10,
        preserve.width = 'common',
        digits = 8
      )
      msg <- sprintf(
        'rtol = %s | nfunk = %s | ytry = %s | fac = %s | y[9] = %s',
        strs[1], strs[2], strs[3], strs[4], strs[5]
      )
      message(msg)
    }
  } else {
    DisplayMessage <- function(rtol, nfunk, ytry, fac, y9) { return(NULL) }
  }

  AmoebaTry <- function(fac) {
    fac1 <- (1.0 - fac) / ndim
    fac2 <- fac1 - fac
    ptry <- psum * fac1 - p[ihi, ] * fac2
    ytry <<- FitLLTotal(ptry, probSurv1996, param, info, data, detailedResults = FALSE)

    if (ytry < y[ihi]) {
      y[ihi] <<- ytry
      psum <<- psum + ptry - p[ihi, ]
      p[ihi, ] <<- ptry
    }

    DisplayMessage(rtol, nfunk, ytry, fac, y[9])
  }

  # Code -------------------------------------------------------------------------------------------

  NMAX <- 50000
  mpts <- ndim + 1
  nfunk <- 0
  psum <- colSums(p)

  seqMpts <- seq_len(mpts)
  seqNDim <- seq_len(ndim)
  ytry <- y

  while (
    nfunk < NMAX
  ) {

    ilo <- 1L
    ihi <- 2L
    inhi <- 1L
    if (y[1] > y[2]) {
      ihi <- 1L
      inhi <- 2L
    }

    for (i in seqMpts) {
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
      Swap1D(y, 1, ilo)
      Swap2D(p, 1, seqNDim, ilo, seqNDim)
      break
    }

    if (nfunk >= NMAX) {
      message('NMAX exceeded')
    }

    nfunk <- nfunk + 2

    # Begin a new iteration. First extrapolate by a factor -1 through the face
    # of the simplex across from the high points, i.e., reflect the simplex from
    # the high point.
    AmoebaTry(-1)

    if (ytry <= y[ilo]) {
      # Gives a result better than the best point, so try an additional
      # extrapolation by a factor 2.
      AmoebaTry(2.0)
    } else if (ytry >= y[inhi]) {
      ysave <- y[ihi]
      AmoebaTry(0.5)

      if (ytry >= ysave) {
        # Can't seem to get rid of that high point. Better contract around the
        # lowest (best) point.
        for (i in seqMpts[seqMpts != ilo]) {
          p[i, ] <- 0.5 * (p[i, ] + p[ilo, ])
          y[i] <- FitLLTotal(p[i, ], probSurv1996, param, info, data, detailedResults = FALSE)
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
  }

  pParam <- p[1, ]
  finalResults <- FitLLTotal(pParam, probSurv1996, param, info, data)

  return(list(
    P = pParam,
    LLTotal = finalResults$LLTotal,
    ModelResults = finalResults$ModelResults
  ))
}
