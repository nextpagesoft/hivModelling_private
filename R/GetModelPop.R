#' GetModelPop
#'
#' Description
#'
#' @param country Country code
#'
#' @return
#' Input data as data.table
#'
#' @examples
#' GetModelPop(country = 'NL')
#' GetModelPop(country = 'PL')
#' GetModelPop()
#'
#' @export
GetModelPop <- function(country = '')
{
  if (is.null(country)) {
    country <- ''
  }

  result <- switch(
    country,
    'NL' = c('ALL', 'MSM', 'IDU', 'HETFI', 'HETMI', 'HETFL', 'HETML'),
    c()
  )

  return(result)
}
