
#' @title Getting User Information from name
#' @description Calls \code{\link{gcite_user_info}} after getting the user
#' identifier
#' @param author author name separated by spaces
#' @param ask If multiple authors are found, should a menu be given
#' @param pagesize Size of pages, max 100, passed to \code{\link{gcite_url}}
#' @param verbose Print diagnostic messages
#' @param secure use https vs. http
#' @param force If passing a URL and there is a failure, should the 
#' program return \code{NULL}, passed to \code{\link{gcite_citation_page}}
#' @param read_citations Should all citation pages be read?
#' @param sleeptime time in seconds between http requests, 
#' to avoid Google Scholar rate limit 
#' @param ... Additional arguments passed to \code{\link{GET}}
#'
#' @return A list of citations, citation indices, and a 
#' \code{data.frame} of authors, journal, and citations, and a 
#' \code{data.frame} of the links to all paper URLs.
#' @export
#'
#' @examples \dontrun{
#' if (!is_travis()) {
#' df = gcite_author_info(author = "John Muschelli", secure = FALSE)
#' }
#' }
#' if (!is_travis() & !is_cran()) {
#' df = gcite_author_info(author = "Jiawei Bai", secure = FALSE)
#' } 
gcite_author_info = function(
  author, 
  ask = TRUE, 
  pagesize = 100, 
  verbose = TRUE, 
  secure = TRUE,
  force = FALSE,
  read_citations = TRUE,
  sleeptime = 0,    
  ...) {
  user = gcite_username(author = author, verbose = verbose, 
                        ask = ask, secure = secure,
                        ...)
  if (is.null(user)) {
    stop("No username found")
  }
  res = gcite_user_info(
    user = user, 
    verbose = verbose, 
    pagesize = pagesize, 
    secure = secure,
    force = force,
    read_citations = read_citations,
    sleeptime = sleeptime,  
    ...)
  return(res)
}