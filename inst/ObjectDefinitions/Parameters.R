list(
  Name = 'Parameters',

  Models = list(
    INCIDENCE = list(
      # Require incidence to be zero at start of epidemic
      StartIncZero = TRUE,

      # Country specific parameters
      Country = NULL,

      # 'POISSON' or 'NEGATIVE BINOMIAL'
      FitDistribution = 'POISSON'
    )
  )
)
