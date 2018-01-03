
#' @title Getting User Information from name
#' @description Calls \code{\link{gcite_user_info}} after getting the user
#' identifier
#' @param author author name separated by spaces
#' @param verbose Verbose diagnostic printing
#' @param ask If multiple authors are found, should a menu be given
#' @param secure use https vs. http
#' @param force If passing a URL and there is a failure, should the 
#' program return \code{NULL}, passed to \code{\link{gcite_citation_page}}
#'  
#' @param ... arguments passed to \code{\link{GET}}
#'
#' @return A list of citations, citation indices, and a 
#' \code{data.frame} of authors, journal, and citations, and a 
#' \code{data.frame} of the links to all paper URLs.
#' @export
#'
#' @examples
#' if (!is_travis()) {
#' df = gcite_author_info(author = "John Muschelli", secure = FALSE)
#' }
gcite_author_info = function(
  author, 
  verbose = TRUE, 
  ask = TRUE, 
  secure = TRUE,
  force = FALSE,
  ...) {
  user = gcite_username(author = author, verbose = verbose, 
                        ask = ask, secure = secure,
                        ...)
  
  return(gcite_user_info(user = user, ...))
}