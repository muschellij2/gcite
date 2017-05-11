#' @title Google Citations URL
#' @description Simple wrapper for adding in pagesize and start values for the page
#'
#' @param url URL of the google citations page
#' @param cstart Starting value for the citation page
#' @param pagesize number of citations to return, max is 100
#'
#' @return A character string
#' @export
#'
#' @importFrom httr build_url parse_url
#' @examples
#' url = "https://scholar.google.com/citations?user=T9eqZgMAAAAJ"
#' gcite_url(url = url, pagesize = 100, cstart = 5)
gcite_url = function(url, cstart = 0, pagesize = 100) {
  hres = httr::parse_url(url)
  hres$query$pagesize = pagesize
  hres$query$cstart = cstart
  url = httr::build_url(hres)
  return(url)
}