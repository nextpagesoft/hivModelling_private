PrintH1 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_h1(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

PrintH2 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_h2(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

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

StartProcess <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  processId <- cli::cli_process_start(CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(processId)
}

EndProcess <- function(
  processId = NULL,
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_process_done(id = processId, CollapseTexts(..., collapse = collapse), .envir = .envir)

  invisible(NULL)
}

PrintBullets <- function(
  items = c(),
  collapse = ' ',
  .envir = parent.frame()
) {
  cli::cli_ul()
  sapply(items, cli::cli_li)
  cli::cli_end()

  invisible(NULL)
}

CollapseTexts <- function(
  ...,
  collapse = ' '
) {
  return(paste(list(...), collapse = collapse))
}
