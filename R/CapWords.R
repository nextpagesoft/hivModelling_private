#' CapWords
#'
#' Capitalizes every word of string
#'
#' @param s String to capitalize. Required.
#' @param strict Logical indicating to make sure only the first letter of output
#'   string is capital. Default = \code{FALSE}
#'
#' @return
#' NULL (invisibly)
#'
#' @examples
#' CapWords(c('word1', 'word2'))
#' CapWords('this is sentence')
#' CapWords('MODEL', strict = FALSE)
#' CapWords('MODEL', strict = TRUE)
#'
#' @export
CapWords <- function(s, strict = FALSE)
{
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
          {s <- substring(s, 2); if (strict) tolower(s) else s},
          sep = '', collapse = ' ')
  }
  result <- sapply(strsplit(s, split = ' '), cap, USE.NAMES = !is.null(names(s)))

  return(result)
}
