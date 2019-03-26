#' GetDefaultRunContex
#'
#' Description
#'
#' @return
#' context list object
#'
#' @examples
#' GetDefaultRunContex()
#'
#' @export
GetDefaultRunContex <- function()
{
  context <- list()

  context[['Parameters']] <- GetObjectDefinition('Parameters')
  context[['Settings']]   <- GetObjectDefinition('Settings')

  return(context)
}
