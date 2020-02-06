ValidateData <- function(
  data
) {

#   // Get range of calculations
#   Regex allSearchRegex = new Regex("^(HIV|AIDS)");
#   ushort allMinYear = 1980;
#   ushort allMaxYear = _incidencePreMinMaxYears.Where(t => allSearchRegex.IsMatch(t.Key)).Select(t => t.Value.Item2).Max();
#   this._incidenceFinalMinMaxYears.Add("All", new Tuple<string, ushort, ushort>("Range of calculations", allMinYear, allMaxYear));
#
#   // Get time interval for HIV diagnoses, by CD4 count
#   Regex cd4SearchRegex = new Regex("^HIV_CD4");
#   List<ushort> cd4MinYears = _incidencePreMinMaxYears.Where(t => cd4SearchRegex.IsMatch(t.Key)).Select(t => t.Value.Item1).ToList();
#   List<ushort> cd4MaxYears = _incidencePreMinMaxYears.Where(t => cd4SearchRegex.IsMatch(t.Key)).Select(t => t.Value.Item2).ToList();
#   if ((cd4MinYears.Count() > 0) && (cd4MaxYears.Count() > 0)) {
#     int allowedStartYear = 1984;
#     int allowedEndYear = allMaxYear;
#
#     int startYear = cd4MinYears.Min();
#     int endYear = cd4MaxYears.Max();
#
#     if ((startYear <= allowedEndYear && allowedStartYear <= endYear) && !(allowedStartYear > allowedEndYear)) {
#       startYear = Math.Max(startYear, allowedStartYear);
#       endYear = Math.Min(endYear, allowedEndYear);
#       allowedEndYear = startYear - 1;
#     } else {
#       // Disable data completely
#       startYear = allMinYear - 1;
#       endYear = allMinYear - 1;
#     }
#     this._incidenceFinalMinMaxYears.Add("HIVCD4", new Tuple<string, ushort, ushort>("HIV diagnoses, by CD4 count", (ushort)startYear, (ushort)endYear));
#
#     // Get time interval for HIV diagnoses, total
#     startYear = _incidencePreMinMaxYears["HIV"].Item1;
#     endYear = _incidencePreMinMaxYears["HIV"].Item2;
#     if ((startYear <= allowedEndYear && allowedStartYear <= endYear) && !(allowedStartYear > allowedEndYear)) {
#       startYear = Math.Max(startYear, allowedStartYear);
#       endYear = Math.Min(endYear, allowedEndYear);
#       allowedEndYear = startYear - 1;
#     } else {
#       // Disable data completely
#       startYear = allMinYear - 1;
#       endYear = allMinYear - 1;
#     }
#     this._incidenceFinalMinMaxYears.Add("HIV", new Tuple<string, ushort, ushort>("HIV diagnoses, total", (ushort)startYear, (ushort)endYear));
#   } else {
#     this._incidenceFinalMinMaxYears.Add("HIVCD4", new Tuple<string, ushort, ushort>("HIV diagnoses, by CD4 count", (ushort)(allMinYear - 1), (ushort)(allMinYear - 1)));
#
#     int allowedStartYear = 1984;
#     int allowedEndYear = allMaxYear;
#
#     // Get time interval for HIV diagnoses, total
#     int startYear = _incidencePreMinMaxYears["HIV"].Item1;
#     int endYear = _incidencePreMinMaxYears["HIV"].Item2;
#     if ((startYear <= allowedEndYear && allowedStartYear <= endYear) && !(allowedStartYear > allowedEndYear)) {
#       startYear = Math.Max(startYear, allowedStartYear);
#       endYear = Math.Min(endYear, allowedEndYear);
#       allowedEndYear = startYear - 1;
#     } else {
#       // Disable data completely
#       startYear = allMinYear - 1;
#       endYear = allMinYear - 1;
#     }
#     this._incidenceFinalMinMaxYears.Add("HIV", new Tuple<string, ushort, ushort>("HIV diagnoses, total", (ushort)startYear, (ushort)endYear));
#   }
#
#   // Get time interval for AIDS diagnoses, total
#   // Disable data completely
#   int aidsStartYear = allMinYear - 1;
#   int aidsEndYear = allMinYear - 1;
#   if (_incidencePreMinMaxYears.ContainsKey("AIDS")) {
#     aidsStartYear = _incidencePreMinMaxYears["AIDS"].Item1;
#     aidsEndYear = Math.Min(_incidencePreMinMaxYears["AIDS"].Item2, (ushort)1995);
#   }
#   _incidenceFinalMinMaxYears.Add("AIDS", new Tuple<string, ushort, ushort>("AIDS diagnoses, total", (ushort)aidsStartYear, (ushort)aidsEndYear));
#
#   // Get time interval for HIV/AIDS diagnoses, total
#   this._incidenceFinalMinMaxYears.Add(
#     "HIVAIDS",
#     new Tuple<string, ushort, ushort>(
#       "HIV/AIDS diagnoses, total",
#       _incidencePreMinMaxYears["HIVAIDS"].Item1,
#       _incidencePreMinMaxYears["HIVAIDS"].Item2));
#
#   // Set fixed intervals
#   this._diagnosisRatesModel = new DiagnosisRatesModel(allMinYear, allMaxYear, 5, 1984);
# }

  return(NULL)
}
