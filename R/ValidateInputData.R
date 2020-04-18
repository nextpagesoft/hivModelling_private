#' ValidateInputData
#'
#' Performs checks on input data
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' \dontrun{
#'   ValidateInputData(dt)
#' }
#'
#' @export
ValidateInputData <- function(
  dt
) {

  # Source: IncidencePopulationDataManager::ValidateSelf

  requriedData <- c('HIV', 'HIVAIDS')
  optionalData <- c('AIDS', 'Dead', 'HIV_CD4_1', 'HIV_CD4_2', 'HIV_CD4_3', 'HIV_CD4_4')

  validationMsgs <- data.table(
    Type = character(),
    Description = character()
  )

  for (dataName in requiredData) {

    #
    # // Get risk categories
    # List<string> riskCategories = (from dc in dt.Columns.Cast<DataColumn>() select dc.ColumnName).Skip(1).ToList();
    # if (this._riskCategories.Count == 0) {
    #   // Initialize the risk groups
    #   this._riskCategories = riskCategories;
    # } else {
    #   // Check if the same risk categories occur in all files
    #   bool sameRiskCategories = new HashSet<string>(this._riskCategories).SetEquals(riskCategories);
    #   if (!sameRiskCategories) {
    #     this.ValidationDetails.AddError("DIFFERENT_RISK_CATEGORIES_" + keyLabel, "File " + fileName + " contains different risk categories than those found in the previously loaded csv files.");
    #     isFileValid = false;
    #   }
    # }
    #
    # // Check the number of columns (should be at least 2)
    # if (dt.Columns.Count < 2) {
    #   this.ValidationDetails.AddError("WRONG_COLUMN_COUNT_" + keyLabel, "File " + fileName + " contains fewer than two columns.");
    #   isFileValid = false;
    # }
    #
    # if (isFileValid) {
    #   List<int> years = dt.AsEnumerable().Select(x => Convert.ToInt32(x[0])).ToList();
    #   List<float> counts = dt.AsEnumerable().Select(x => Convert.ToSingle(x[1])).ToList();
    #
    #   if (years.Count == 0) {
    #     this.ValidationDetails.AddError("FILE_EMPTY_" + keyLabel, "Required file " + fileName + " contains no records.");
    #     isFileValid = false;
    #   } else {
    #     // First column should contain integers only (years between MINYEAR and MAXYEAR, come from Settings)
    #     minYear = Math.Max(MINYEAR, years.Min());
    #     maxYear = Math.Min(MAXYEAR, years.Max());
    #
    #     // Check if Start year <= End year
    #     if (minYear > maxYear) {
    #       this.ValidationDetails.AddMessage("YEARS_OUTSIDE_RANGE_" + keyLabel, "File " + fileName + " has start year and/or end year outside of range [" + MINYEAR + ", " + MAXYEAR + "].");
    #       isFileValid = false;
    #     } else {
    #       // Check if year sequence is in increasing order
    #       bool isOrdered = years.Skip(1).Select((v, i) => v > years[i]).All(v => v);
    #       if (!isOrdered) {
    #         this.ValidationDetails.AddError("YEARS_NOT_IN_ORDER_" + keyLabel, "Years in file " + fileName + " are not in an increasing order.");
    #         isFileValid = false;
    #       }
    #
    #       // Check if there are missing years
    #       List<int> missingYears = Enumerable.Range(minYear, maxYear - minYear + 1).Except(years).ToList<int>();
    #       if (missingYears.Count > 0)
    #         this.ValidationDetails.AddMessage("MISSING_YEARS_" + keyLabel, "File " + fileName + " does not provide data for " + missingYears.Count + " year(s).");
    #
    #       // Check if all counts are non-negative
    #       bool areAllNonNegative = counts.All(v => v >= 0);
    #       if (!areAllNonNegative) {
    #         this.ValidationDetails.AddError("NEGATIVE_COUNT_" + keyLabel, "File " + fileName + " contains negative counts.");
    #         isFileValid = false;
    #       }
    #     }
    #   }
    # }


  }

  for (dataName in optionalData) {

  }

  invisible(NULL)
}
