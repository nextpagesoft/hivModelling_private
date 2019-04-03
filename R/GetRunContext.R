#' GetRunContext
#'
#' Description
#'
#' @param ... Parameters to be merged with default parameters and settings.
#'   Optional.
#'
#' @return
#' context list object
#'
#' @examples
#' GetRunContext()
#'
#' @export
GetRunContext <- function(...)
{
  args <- list(...)

  if (length(names(args)) > 0) {
    names(args) <- CapWords(names(args))
    stopifnot(names(args) %in% c('Settings', 'Parameters'))
  }

  defaultContext <- list(
    Settings   = GetObjectDefinition('Settings'),
    Parameters = GetObjectDefinition('Parameters')
  )

  context <- modifyList(defaultContext, args)

  return(context)
}
