library(data.table)
library(hivModelling)
library(hivEstimatesAccuracy)

# caseBasedData <- fread('~/share/HIV Bootstrap/Dummy_case_based.csv')
# hivModelDataSets <- PrepareDataSetsForModel(caseBasedData, by = c('Gender', 'Transmission'))
# saveRDS(hivModelDataSets, file = '~/share/HIV Bootstrap/Dummy_aggregated.rds')
# hivModelDataSets <- readRDS(file = '~/share/HIV Bootstrap/Dummy_aggregated.rds')

caseBasedData <- fread('~/share/HIV Bootstrap/BE_case_based.csv')
hivModelDataSets <- PrepareDataSetsForModel(caseBasedData, by = c('Gender', 'Transmission'))
WriteZipFile(hivModelDataSets, '~/share/HIV Bootstrap/BE_aggregated.zip')
# saveRDS(hivModelDataSets, file = '~/share/HIV Bootstrap/BE_aggregated.rds')
# hivModelDataSets <- readRDS(file = '~/share/HIV Bootstrap/BE_aggregated.rds')

context <- GetRunContext(
  data = hivModelDataSets[[1]],
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
      Intervals = data.table(
        StartYear = c(1980L, 1984L, 1992L, 2000L, 2008L),
        Jump = c(FALSE, TRUE, TRUE, TRUE, TRUE),
        ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      )
    )
  )
)

populationData <- GetPopulationData(context)

mainResults <- PerformMainFit(context, populationData)

plots <- CreateOutputPlots(mainResults)
