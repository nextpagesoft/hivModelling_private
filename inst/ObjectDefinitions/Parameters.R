list(
  Name = 'Parameters',

  Models = list(
    INCIDENCE = list(
      # Set country for country specific parameters
      Country = NULL,

      FullModel = TRUE,

      ModelMinYear = 1980,

      ModelMaxYear = 2017,

      FitPosMinYear = 1979,

      FitPosMaxYear = 1979,

      FitPosCD4MinYear = 1984,

      FitPosCD4MaxYear = 2016,

      FitAIDSPosMinYear = 1996,

      FitAIDSPosMaxYear = 2016,

      FitAIDSMinYear = 1980,

      FitAIDSMaxYear = 1995,

      # Intervals for diagnosis matrix
      Intervals = data.table(
        StartYear = c(1980L, 1984L, 1996L, 2000L, 2005L, 2010L),
        EndYear = c(1984L, 1996L, 2000L, 2005L, 2010L, 2017L),
        Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
      ),

      # 'POISSON' or 'NEGATIVE_BINOMIAL'
      FitDistribution = 'NEGATIVE_BINOMIAL',

      # Over-dispersion
      RDisp = 50,

      # Incidence curve --------------------------------------------------------

      # Spline: number of knots
      ModelNoKnots = 6,

      # Require incidence to be zero at start of epidemic
      StartIncZero = TRUE,

      # B-splines: smooth incidence curve at the end of the observation interval
      SplineType = 'B-SPLINE',

      # Non editable model parameters ------------------------------------------

      SplineOrder = 4,

      # Number of disease stages
      NoStage = 5,

      # Rates of progression to AIDS through stages of CD4
      FInit = c(0.58, 0.23, 0.16, 0.03, 0),

      Qoppa = c(1 / 6.37, 1 / 2.86, 1 / 3.54, 1 / 2.3, 0.529101),

      # Background mortality
      Mu = 0,

      AlphaP = 1.0 / (2.90 / 12),

      Delta4Fac = 0,

      DeltaAIDS = 12,

      # Correction for incidence at end of observation interval (TRUE = yes;
      # FALSE = no) by extending spline base beyond the maximum year and fixing
      # the parameter associated with the last spline function to 0.
      MaxIncCorr = TRUE,

      NoDelta = 1,

      NoTheta = 8,

      NoThetaFix = 0,

      Smoothing1 = 0,

      Smoothing2 = 0,

      # Annual intervals time diagnosis
      DefNoDiagTime = 15
    )
  )
)
