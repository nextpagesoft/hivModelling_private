list(
  Name = 'Parameters',

  Models = list(
    INCIDENCE = list(

      # Editable parameters ------------------------------------------------------------------------
      ModelMinYear = 1980L,

      ModelMaxYear = 2016L,

      FitPosMinYear = 1979L,

      FitPosMaxYear = 1979L,

      FitPosCD4MinYear = 1984L,

      FitPosCD4MaxYear = 2016L,

      FitAIDSMinYear = 1980L,

      FitAIDSMaxYear = 1995L,

      FitAIDSPosMinYear = 1996L,

      FitAIDSPosMaxYear = 2016L,

      # Intervals for diagnosis matrix
      Intervals = data.table(
        StartYear = c(1980L, 1984L, 1996L, 2000L, 2005L, 2010L),
        Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
        ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
      ),

      # Set country for country specific parameters
      Country = NULL,

      # Spline: number of knots
      ModelNoKnots = 6,

      # Require incidence to be zero at start of epidemic
      StartIncZero = TRUE,

      # 'POISSON' or 'NEGATIVE_BINOMIAL'
      FitDistribution = 'NEGATIVE_BINOMIAL',

      # Over-dispersion
      RDisp = 50,

      Delta4Fac = 0,

      # Correction for incidence at end of observation interval (TRUE = yes;
      # FALSE = no) by extending spline base beyond the maximum year and fixing
      # the parameter associated with the last spline function to 0.
      MaxIncCorr = TRUE,

      # B-splines: smooth incidence curve at the end of the observation interval
      SplineType = 'B-SPLINE',

      FullData = TRUE,

      # Non editable parameters --------------------------------------------------------------------

      SplineOrder = 4,

      # Number of disease stages
      NoStage = 5,

      # Rates of progression to AIDS through stages of CD4
      FInit = c(0.58, 0.23, 0.16, 0.03, 0),

      Qoppa = c(1 / 6.37, 1 / 2.86, 1 / 3.54, 1 / 2.3, 0.529101),

      # Background mortality
      Mu = 0,

      AlphaP = 1.0 / (2.90 / 12),

      DeltaAIDS = 12,

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
