library(data.table)
library(hivModelling)
library(hivEstimatesAccuracy)

# Load case-based data set with no missings
caseBasedData <- fread('~/share/HIV Bootstrap/BE_case_based.csv')

# LOOP

# 1. Boostrap case-based data
indices <- sample(nrow(caseBasedData), replace = TRUE)
sampleCaseBasedData <- caseBasedData[indices]

# 2. Create aggregated data set
hivModelDataSet <- PrepareDataSetsForModel(
  sampleCaseBasedData,
  strata = c('Gender', 'Transmission'),
  splitBy = NULL
)

# Create context
context <- GetRunContext(
  data = hivModelDataSet,
  parameters = list(
    INCIDENCE = list(
      ModelMinYear = 1980,
      ModelMaxYear = 2017,
      FitPosMinYear = 1979,
      FitPosMaxYear = 1979,
      FitPosCD4MinYear = 1985,
      FitPosCD4MaxYear = 2015,
      FitAIDSMinYear = 1985,
      FitAIDSMaxYear = 1995,
      FitAIDSPosMinYear = 1985,
      FitAIDSPosMaxYear = 2015,
      ModelNoKnots = 4,
      FitDistribution = 'POISSON',
      Intervals = data.table(
        StartYear = c(1980L, 1984L, 1992L, 2000L, 2008L),
        Jump = c(FALSE, TRUE, TRUE, TRUE, TRUE),
        ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      )
    )
  )
)

# 4. Create final data set for the model
data <- GetPopulationData(context)

# 5. Fit the model
mainResults <- PerformMainFit(context, data)

# 6. Create plots
plots <- CreateOutputPlots(mainResults)
