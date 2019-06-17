#' GetObjectDefinition
#'
#' Returns object definition stored as text file.
#'
#' @param objectName Name of the object. Required.
#' @param path Path to the object definition file. Optional.
#'   Default = \code{\link{system.file}('ObjectDefinitions',
#'   package = 'hivModelling')}.
#' @param section Name of section of the list to return. If not specified, then
#'   the whole specification is returned, otherwise only the specified section.
#'   Optional. Default = \code{NULL}.
#' @param includeFileName Logical indicating to include file name in the
#'   returned list as field 'FileName'. Optional. Default = \code{FALSE}.
#'
#' @return List object.
#'
#' @examples
#' \dontrun{
#' GetObjectDefinition(objectName = 'Model')
#' GetObjectDefinition(objectName = 'Model', section = 'Name')
#' GetObjectDefinition(objectName = 'Settings', section = 'Columns')
#' }
#'
#' @export
GetObjectDefinition <- function(
  objectName,
  path = system.file('ObjectDefinitions', package = 'hivModelling'),
  section = NULL,
  includeFileName = FALSE
) {
  stopifnot(!missing(objectName))

  fileName <- file.path(path,
                        paste(objectName, 'R', sep = '.'))

  # Get entire list
  listObject <- eval(expr = parse(file = fileName))
  if (includeFileName) {
    listObject$FileName <- fileName
  }

  # Return only a specific section of the list
  if (!is.null(section)) {
    if (section %in% names(listObject)) {
      listObject <- listObject[[section]]
    }
  }

  return(listObject)
}
