#' PrintH1
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH1('Test')
#'
#' @export
PrintH1 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_h1(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

#' PrintH2
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH2('Test')
#'
#' @export
PrintH2 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_h2(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

#' PrintAlert
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param type Type of alert
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintAlert('Test')
#' PrintAlert('Test', type = 'success')
#' PrintAlert('Test', type = 'danger')
#' PrintAlert('Test', type = 'warning')
#' PrintAlert('Test', type = 'info')
#'
#' @export
PrintAlert <- function(
  ...,
  collapse = ' ',
  type = 'info',
  .envir = parent.frame()
) {

  alertFunc <- switch(
    tolower(type),
    'danger'  = cli::cli_alert_danger,
    'warning' = cli::cli_alert_warning,
    'info'    = cli::cli_alert_info,
    'success' = cli::cli_alert_success,
    cli::cli_alert
  )

  alertFunc(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

#' StartProcess
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' processId <- StartProcess('Test')
#' }
#'
#' @export
StartProcess <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  try(cli::cli_status_clear(NULL), silent = TRUE)
  processId <- cli::cli_process_start(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(processId)
}

#' EndProcess
#'
#' @param processId Process Id
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' EndProcess(processId, 'Test')
#' }
#'
#' @export
EndProcess <- function(
  processId = NULL,
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_process_done(id = processId, CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

#' PrintBullets
#'
#' @param items Vector of text items
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintBullets(c('Item 1', 'Item 2'))
#'
#' @export
PrintBullets <- function(
  items = c(),
  .envir = parent.frame()
) {
  cli::cli_ul()
  sapply(items, cli::cli_li)
  cli::cli_end()

  invisible(NULL)
}

#' CollapseTexts
#'
#' @param ... Text to be collapsed
#' @param collapse String to be used for concatenating texts
#'
#' @return NULL
#'
#' @examples
#' CollapseTexts('Item 1', 'Item 2')
#'
#' @export
CollapseTexts <- function(
  ...,
  collapse = ' '
) {
  return(paste(list(...), collapse = collapse))
}
