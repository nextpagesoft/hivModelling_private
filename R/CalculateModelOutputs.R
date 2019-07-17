CalculateModelOutputs <- function(
  modelResults,
  info,
  param
) {
  VERY_SML <- 1.0e-20
  bitSml <- 1e-6
  eps <- 0.0001
  hMin <- 0
  h1 <- 0.02

  numYears <- nrow(modelResults)

  PI_Now_2D_DV <- matrix(0, numYears, numYears)
  CD4_Now_U_2D_DV <- array(0, dim = c(numYears, numYears, param$NoStage))
  CD4_Now_D_2D_DV <- array(0, dim = c(numYears, numYears, param$NoStage))
  CD4_Cum_D_2D_DV <- array(0, dim = c(numYears, numYears, param$NoStage))
  N_Und_1 <- rep(0, numYears)
  N_Und_2 <- rep(0, numYears)
  PI_Time <- matrix(0, 3, numYears)
  CD4_U_Time <- array(0, dim = c(3, numYears, param$NoStage))
  CD4_D_Time <- array(0, dim = c(3, numYears, param$NoStage))
  U_Time <- matrix(0, 3, numYears)
  D_Time <- matrix(0, 3, numYears)
  D_Cum_Time <- matrix(0, param$DefNoDiagTime, numYears)
  D_Cum_Time_Late <- matrix(0, param$DefNoDiagTime, numYears)
  D_Cum_Time_Advanced <- matrix(0, param$DefNoDiagTime, numYears)
  D_Avg_Time <- rep(0, numYears)

  for (i in seq_len(numYears)) {
    ystart <- rep(0, param$NoEq)
    tmpMinYear <- info$ModelMinYear + (i - 1)
    tmpMaxYear <- tmpMinYear + 1
    for (j in seq(from = i, to = numYears)) {
      timeA <- tmpMinYear + (j - i)
      timeB <- timeA + 1

      res <- odeint(ystart,
                    nVar = param$NoEq,
                    x1 = timeA + bitSml,
                    x2 = timeB - bitSml,
                    eps,
                    h1,
                    hMin,
                    param,
                    info,
                    minYear = tmpMinYear,
                    maxYear = tmpMaxYear,
                    derivsFunc = derivsFunc_c)
      ystart <- res$YStart
      iEq <- 1

      # Number in primary infection (PI) at the end of year j of those infected in year i
      PI_Now_2D_DV[i, j] <- ystart[iEq]
      iEq <- iEq + 1

      # Number undiagnosed (U) who are in stage k at the end of year j of those infected in year i
      CD4_Now_U_2D_DV[i, j, ] <- ystart[seq_len(param$NoStage) + iEq - 1]
      iEq <- iEq + param$NoStage

      # Number diagnosed (D) who are in stage k at the end of year j of those infected in year i
      CD4_Now_D_2D_DV[i, j, ] <- ystart[seq_len(param$NoStage) + iEq - 1]
      iEq <- iEq + param$NoStage

      # Cumulative number diagnosed in stage k by the end of year j of those infected in year i
      CD4_Cum_D_2D_DV[i, j, ] <- ystart[seq_len(param$NoStage) + iEq - 1]
    }

    if (numYears >= i + 1) {
      indices <- seq(from = numYears, to = i + 1)
    } else {
      indices <- c()
    }
    for (j in indices) {
      CD4_Cum_D_2D_DV[i, j, ] <- CD4_Cum_D_2D_DV[i, j, ] - CD4_Cum_D_2D_DV[i, j - 1, ]
    }
  }

  # Calculate the number of undiagnosed individuals who were infected before (N_Und_1)
  # or after (N_Und_2) FitMinYear
  # i <- 1
  for (i in seq_along(modelResults$Year)) {
    # j <- i
    for (j in seq(i, length(modelResults$Year))) {
      if (modelResults$Year[i] >= info$FitMinYear) {
        # Number undiagnosed in year j and infected in year i or later
        N_Und_2[j] <- PI_Now_2D_DV[i, j] + sum(CD4_Now_U_2D_DV[i, j, ])
      } else {
        # Number undiagnosed in year j and infected before year i
        N_Und_1[j] <- PI_Now_2D_DV[i, j] + sum(CD4_Now_U_2D_DV[i, j, ])
      }
    }
  }

  # sel <- modelResults$Year < info$FitMinYear
  # if (any(sel)) {
  #   # Number undiagnosed in year j and infected before year i
  #   N_Und_1[sel] <- rowSums(PI_Now_2D_DV[sel, ]) + rowSums(apply(CD4_Now_U_2D_DV[sel, , ], 1, rowSums))
  # }
  # if (any(!sel)) {
  #   # Number undiagnosed in year j and infected in year i or later
  #   N_Und_2[!sel] <- rowSums(PI_Now_2D_DV[!sel, ]) + rowSums(apply(CD4_Now_U_2D_DV[!sel, , ], 1, rowSums))
  # }

  # For each year calculate the number in primary infection and in each CD4 stratum (undiagnosed
  # or diagnosed) stratified by (1) infected in same year, (2) infected 1 to 4 years before, or (3)
  # more than 4 years before.
  for (year in seq_len(numYears)) {
    PI_Time[1, year] <- PI_Now_2D_DV[year, year]
    U_Time[1, year] <- PI_Now_2D_DV[year, year]

    CD4_U_Time[1, year, ] <- CD4_Now_U_2D_DV[year, year, ]
    U_Time[1, year] <- U_Time[1, year] + sum(CD4_Now_U_2D_DV[year, year, ])
    D_Time[1, year] <- D_Time[1, year] + sum(CD4_Now_D_2D_DV[year, year, ])

    j <- year
    while (j > 1) {
      j <- j - 1
      if (j >= year - 4) {
        PI_Time[2, year] <- PI_Time[2, year] + PI_Now_2D_DV[j, year]
        U_Time[2, year] <- U_Time[2, year] + PI_Now_2D_DV[j, year]
        CD4_U_Time[2, year, ] <- CD4_U_Time[2, year, ] + CD4_Now_U_2D_DV[j, year, ]
        U_Time[2, year] <- U_Time[2, year] + sum(CD4_Now_U_2D_DV[j, year, ])
        D_Time[2, year] <- D_Time[2, year] + sum(CD4_Now_D_2D_DV[j, year, ])
      } else {
        PI_Time[3, year] <- PI_Time[3, year] + PI_Now_2D_DV[j, year]
        U_Time[3, year] <- U_Time[3, year] + PI_Now_2D_DV[j, year]
        CD4_U_Time[3, year, ] <- CD4_U_Time[3, year, ] + CD4_Now_U_2D_DV[j, year, ]
        U_Time[3, year] <- U_Time[3, year] + sum(CD4_Now_U_2D_DV[j, year, ])
        D_Time[3, year] <- D_Time[3, year] + sum(CD4_Now_D_2D_DV[j, year, ])
      }
    }

    D_Cum_Time[1, year] <- D_Cum_Time[1, year] + sum(CD4_Cum_D_2D_DV[year, year, ])
    D_Avg_Time[year] <- D_Avg_Time[year] + sum(CD4_Cum_D_2D_DV[year, year, ]) * 0.5

    D_Cum_Time_Advanced[1, year] <-
      CD4_Cum_D_2D_DV[year, year, param$NoStage] +
      CD4_Cum_D_2D_DV[year, year, param$NoStage - 1]
    D_Cum_Time_Late[1, year] <-
      D_Cum_Time_Advanced[1, year] +
      CD4_Cum_D_2D_DV[year, year, param$NoStage - 2]

    j <- year
    l <- 1
    while (j > 1) {
      j <- j - 1
      l <- l + 1
      if (j > year - param$DefNoDiagTime) {
        D_Cum_Time_Advanced[l, year] <-
          D_Cum_Time_Advanced[l, year] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage - 1]
        D_Cum_Time_Late[l, year] <-
          D_Cum_Time_Late[l, year] +
          D_Cum_Time_Advanced[l, year] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage - 2]
      } else {
        D_Cum_Time_Advanced[param$DefNoDiagTime, year] <-
          D_Cum_Time_Advanced[param$DefNoDiagTime, year] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage - 1]
        D_Cum_Time_Late[param$DefNoDiagTime, year] <-
          D_Cum_Time_Late[param$DefNoDiagTime, year] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage - 1] +
          CD4_Cum_D_2D_DV[j, year, param$NoStage - 2]
      }

      for (k in seq_len(param$NoStage)) {
        if (j > year - param$DefNoDiagTime) {
          D_Cum_Time[l, year] <- D_Cum_Time[l, year] + CD4_Cum_D_2D_DV[j, year, k]
        } else {
          D_Cum_Time[param$DefNoDiagTime, year] <-
            D_Cum_Time[param$DefNoDiagTime, year] +
            CD4_Cum_D_2D_DV[j, year, k]
        }
      }
      D_Avg_Time[year] <- D_Avg_Time[year] + sum(CD4_Cum_D_2D_DV[j, year, ]) * (l - 0.5)
    }
  }
  D_Avg_Time <- D_Avg_Time / (modelResults$N_HIV + VERY_SML)

  return(list(
    PI_Now_2D_DV = PI_Now_2D_DV,
    CD4_Now_U_2D_DV = CD4_Now_U_2D_DV,
    CD4_Now_D_2D_DV = CD4_Now_D_2D_DV,
    CD4_Cum_D_2D_DV = CD4_Cum_D_2D_DV,
    N_Und_1 = N_Und_1,
    N_Und_2 = N_Und_2,
    PI_Time = PI_Time,
    CD4_U_Time = CD4_U_Time,
    CD4_D_Time = CD4_D_Time,
    U_Time = U_Time,
    D_Time = D_Time,
    D_Cum_Time = D_Cum_Time,
    D_Cum_Time_Late = D_Cum_Time_Late,
    D_Cum_Time_Advanced = D_Cum_Time_Advanced,
    D_Avg_Time = D_Avg_Time
  ))
}
