#' PrintH1
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PrintH1('Test')
#' }
PrintH1 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame(),
  verbose = TRUE
) {
  if (verbose) {
    cli::cli_h1(CollapseTexts(..., collapse = collapse), .envir = .envir)
  }

  invisible(NULL)
}

#' PrintH2
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PrintH2('Test')
#' }
PrintH2 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame(),
  verbose = TRUE
) {
  if (verbose) {
    cli::cli_h2(CollapseTexts(..., collapse = collapse), .envir = .envir)
  }

  invisible(NULL)
}

#' PrintH3
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PrintH3('Test')
#' }
PrintH3 <- function(
                    ...,
                    collapse = ' ',
                    .envir = parent.frame(),
                    verbose = TRUE) {
  if (verbose) {
    cli::cli_h3(CollapseTexts(..., collapse = collapse), .envir = .envir)
  }

  invisible(NULL)
}

#' PrintAlert
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param type Type of alert
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PrintAlert('Test')
#' PrintAlert('Test', type = 'success')
#' PrintAlert('Test', type = 'danger')
#' PrintAlert('Test', type = 'warning')
#' PrintAlert('Test', type = 'info')
#' }
PrintAlert <- function(
  ...,
  collapse = ' ',
  type = 'info',
  .envir = parent.frame(),
  verbose = TRUE
) {
  if (verbose) {
    alertFunc <- switch(
      tolower(type),
      'danger'  = cli::cli_alert_danger,
      'warning' = cli::cli_alert_warning,
      'info'    = cli::cli_alert_info,
      'success' = cli::cli_alert_success,
      cli::cli_alert
    )

    alertFunc(CollapseTexts(..., collapse = collapse), .envir = .envir)
  }

  invisible(NULL)
}

#' StartProcess
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' processId <- StartProcess('Test')
#' }
StartProcess <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame(),
  verbose = TRUE
) {
  try(cli::cli_status_clear(NULL), silent = TRUE)
  if (verbose) {
    processId <- cli::cli_process_start(CollapseTexts(..., collapse = collapse), .envir = .envir)
  } else {
    processId <- NULL
  }

  invisible(processId)
}

#' EndProcess
#'
#' @param processId Process Id
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' EndProcess(processId, 'Test')
#' }
EndProcess <- function(
  processId = NULL,
  ...,
  collapse = ' ',
  .envir = parent.frame(),
  verbose = TRUE
) {
  if (verbose & !is.null(processId)) {
    cli::cli_process_done(id = processId, CollapseTexts(..., collapse = collapse), .envir = .envir)
  }

  invisible(NULL)
}

#' PrintBullets
#'
#' @param items Vector of text items
#' @param .envir Environment for lookup of variables referenced in the text
#' @param verbose Logical indicating to print out messages (TRUE) or not (FALSE)
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PrintBullets(c('Item 1', 'Item 2'))
#' }
PrintBullets <- function(
  items = c(),
  .envir = parent.frame(),
  verbose = TRUE
) {
  if (verbose) {
    cli::cli_ul()
    sapply(items, cli::cli_li)
    cli::cli_end()
  }

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
#' \dontrun{
#' CollapseTexts('Item 1', 'Item 2')
#' }
CollapseTexts <- function(
  ...,
  collapse = ' '
) {
  texts <- list(...)
  if (length(texts) > 0) {
    result <- paste(texts, collapse = collapse)
  } else {
    result <- ''
  }
  return(result)
}
