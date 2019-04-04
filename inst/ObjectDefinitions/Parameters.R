list(
  Name = 'Parameters',

  Models = list(
    INCIDENCE = list(
      # Require incidence to be zero at start of epidemic
      StartIncZero = TRUE,

      # Set country for country specific parameters
      Country = NULL,

      # Country specific populations
      ModelPop = c(),

      # 'POISSON' or 'NEGATIVE BINOMIAL'
      FitDistribution = 'POISSON',

      # Diagnosis rate
      Delta = list(

      ),

      # Spline base
      Theta = list(

      )
    )
  )
)
