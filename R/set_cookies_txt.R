
#' Set Cookies from Text file
#'
#' @param file tab-delimited text file of cookies, to be read in using
#' \code{\link{readLines}}.  Comments should start the line with the pound
#' symbol
#' @note 
#' This function searches for domains that contain the word \code{"scholar"}
#' @return Either \code{NULL} if no domains contain the word \code{"scholar"},
#' or an object of class \code{request} from \code{\link{set_cookies}}
#' @importFrom httr set_cookies
# #' @export
set_cookies_txt = function(file) {
  
  x = readLines(file, warn = FALSE)
  x = trimws(x)
  x = x[ !grepl("^#", x)]
  
  xx = strsplit(
    x, split = "\t")
  n = 7
  f = function(x) {
    x = c(x, rep("", length = max(n - length(x), 0)))
  }
  xx = t(matrix(sapply(xx, f), nrow = n))

  
  colnames(xx) = c(
    "domain", "flag", "path", "secure", 
    "expiration", "name", "value")
  xx = as.data.frame(xx, stringsAsFactors = FALSE)
  xx = xx[ grepl("scholar", tolower(xx$domain)), ]
  ret = NULL
  if (nrow(xx) > 0) {
    vals = xx$value
    names(vals) = xx$name
    ret = httr::set_cookies(.cookies = vals)
  } 
  return(ret)
}