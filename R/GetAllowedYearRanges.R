#' GetAllowedYearRanges
#'
#' Get allowed year ranges for incidence data
#'
#' @param data List of incidence data items
#'
#' @return
#' Named list of vectors of year ranges per data item
#'
#' @examples
#' \dontrun{
#' GetAllowedYearRanges(data)
#' }
#'
#' @export
GetAllowedYearRanges <- function(
  data
) {
  # CRAN checks
  Year <- NULL

  if (is.null(data)) {
    return(NULL)
  }

  incidenceFinalMinMaxYears <- list(
    All = c(1980L, 1979L),
    HIV = c(1979L, 1979L),
    AIDS = c(1979L, 1979L),
    HIVAIDS = c(1979L, 1979L),
    HIV_CD4 = c(1979L, 1979L),
    Dead = c(1979L, 1979L)
  )
  incidencePreMinMaxYears <- suppressWarnings(
    lapply(data, function(dt) dt[, c(min(Year), max(Year))])
  )
  incidencePreMinMaxYears <- Filter(Negate(is.null), incidencePreMinMaxYears)

  # Get range of calculations
  allSearchNamesSet <- grepl('^(HIV|AIDS)', names(incidencePreMinMaxYears))

  allMinYear <- 1980
  if (any(allSearchNamesSet)) {
    allMaxYear <- max(sapply(incidencePreMinMaxYears[allSearchNamesSet], '[[', 2))
  } else {
    allMaxYear <- allMinYear - 1
  }

  incidenceFinalMinMaxYears[['All']] <- c(allMinYear, allMaxYear)

  # Get time interval for HIV diagnoses, by CD4 count
  cd4SearchNamesSet <- grepl('HIV_CD4', names(incidencePreMinMaxYears))
  cd4MinYears <- sapply(incidencePreMinMaxYears[cd4SearchNamesSet], '[[', 1)
  cd4MaxYears <- sapply(incidencePreMinMaxYears[cd4SearchNamesSet], '[[', 2)

  if (length(cd4MinYears) > 0 && length(cd4MaxYears) > 0) {
    allowedStartYear <- 1984
    allowedEndYear <- allMaxYear

    startYear <- min(cd4MinYears)
    endYear <- max(cd4MaxYears)

    # Adjust years if needed
    if (
      (startYear <= allowedEndYear && allowedStartYear <= endYear) &&
      !(allowedStartYear > allowedEndYear)
    ) {
      startYear <- max(startYear, allowedStartYear)
      endYear <- min(endYear, allowedEndYear)
      allowedEndYear <- startYear - 1
    } else {
      # Disable data completely
      startYear <- allMinYear - 1
      endYear <- allMinYear - 1
    }
    incidenceFinalMinMaxYears[['HIVCD4']] <- c(startYear, endYear)

    # Get time interval for HIV diagnoses, total
    startYear <- incidencePreMinMaxYears[['HIV']][[1]]
    endYear <- incidencePreMinMaxYears[['HIV']][[2]]
    if (
      (startYear <= allowedEndYear && allowedStartYear <= endYear) &&
      !(allowedStartYear > allowedEndYear)
    ) {
      startYear <- max(startYear, allowedStartYear)
      endYear <- min(endYear, allowedEndYear)
      allowedEndYear <- startYear - 1
    } else {
      # Disable data completely
      startYear <- allMinYear - 1
      endYear <- allMinYear - 1
    }
    incidenceFinalMinMaxYears[['HIV']] <- c(startYear, endYear)
  } else {
    incidenceFinalMinMaxYears[['HIVCD4']] <- c(allMinYear - 1, allMinYear - 1)

    allowedStartYear <- 1984
    allowedEndYear <- allMaxYear

    # Get time interval for HIV diagnoses, total
    startYear <- incidencePreMinMaxYears[['HIV']][[1]]
    endYear <- incidencePreMinMaxYears[['HIV']][[2]]
    if (
      !is.null(startYear) && !is.null(endYear) &&
      (startYear <= allowedEndYear && allowedStartYear <= endYear) &&
      !(allowedStartYear > allowedEndYear)
    ) {
      startYear <- max(startYear, allowedStartYear)
      endYear <- min(endYear, allowedEndYear)
      allowedEndYear <- startYear - 1
    } else {
      # Disable data completely
      startYear <- allMinYear - 1
      endYear <- allMinYear - 1
    }
    incidenceFinalMinMaxYears[['HIV']] <- c(startYear, endYear)
  }

  # Get time interval for AIDS diagnoses, total
  # Disable data completely
  aidsStartYear <- allMinYear - 1
  aidsEndYear <- allMinYear - 1
  if ('AIDS' %in% names(incidencePreMinMaxYears)) {
    aidsEndYear <- min(incidencePreMinMaxYears[['AIDS']][[2]], 1995)
    aidsStartYear <- min(incidencePreMinMaxYears[['AIDS']][[1]], aidsEndYear)
  }
  incidenceFinalMinMaxYears[['AIDS']] <- c(aidsStartYear, aidsEndYear)

  # Get time interval for HIV/AIDS diagnoses, total
  incidenceFinalMinMaxYears[['HIVAIDS']] <- incidencePreMinMaxYears[['HIVAIDS']]

  return(incidenceFinalMinMaxYears)
}
