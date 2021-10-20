#' PrintH1
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts. Default = ' '
#' @param verbose Logical indicating to print the message. Default = TRUE.
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH1('Test')
#' @export
PrintH1 <- function(
  ...,
  collapse = ' ',
  verbose = TRUE,
  .envir = parent.frame()
) {
  if (verbose) {
    formattedText <- capt0(cli::cli_h1(CollapseTexts(..., collapse = collapse), .envir = .envir))
    cat(formattedText)
  }

  invisible(NULL)
}

#' PrintH2
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts. Default = ' '
#' @param verbose Logical indicating to print the message. Default = TRUE.
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH2('Test')
#' @export
PrintH2 <- function(
  ...,
  collapse = ' ',
  verbose = TRUE,
  .envir = parent.frame()
) {
  if (verbose) {
    formattedText <- capt0(cli::cli_h2(CollapseTexts(..., collapse = collapse), .envir = .envir))
    cat(formattedText)
  }

  invisible(NULL)
}

#' PrintAlert
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts. Default = ' '
#' @param type Type of alert
#' @param verbose Logical indicating to print the message. Default = TRUE.
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
#' @export
PrintAlert <- function(
  ...,
  collapse = ' ',
  type = 'info',
  verbose = TRUE,
  .envir = parent.frame()
) {
  if (verbose) {
    alertFunc <- switch(tolower(type),
      'danger'  = cli::cli_alert_danger,
      'warning' = cli::cli_alert_warning,
      'info'    = cli::cli_alert_info,
      'success' = cli::cli_alert_success,
      cli::cli_alert
    )
    formattedText <- capt0(alertFunc(CollapseTexts(..., collapse = collapse), .envir = .envir))
    cat(formattedText)
  }

  invisible(NULL)
}

#' PrintBullets
#'
#' @param items Vector of text items
#' @param verbose Logical indicating to print the message. Default = TRUE.
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintBullets(items = c('Item 1', 'Item 2', 'Item  3', 'beta[ 1] =  0.4'))
#' @export
PrintBullets <- function(
  items = c(),
  verbose = TRUE,
  .envir = parent.frame()
) {
  if (verbose) {
    formattedText <- capt0({
      cli::cat_bullet(items, bullet = 'dot', bullet_col = 'green')
    })
    cat(formattedText)
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
#' CollapseTexts('Item 1', 'Item 2')
#' @export
CollapseTexts <- function(
  ...,
  collapse = ' '
) {
  return(paste(list(...), collapse = collapse))
}

capture_messages <- function(expr) {
  msgs <- character()
  i <- 0
  suppressMessages(withCallingHandlers(
    expr,
    message = function(e) msgs[[i <<- i + 1]] <<- conditionMessage(e)
  ))
  paste0(msgs, collapse = "")
}

capt0 <- function(expr, strip_style = FALSE) {
  out <- capture_messages(expr)
  if (strip_style) cli::ansi_strip(out) else out
}
