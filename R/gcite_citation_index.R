#' @title Parse Google Citation Index
#' @description Parses a google citation indices (h-index, etc.) from main page
#'
#' @param doc A xml_document or the url for the main page
#' @param ... not currently used
#'
#' @return A matrix of indices
#' @export
#' @importFrom rvest html_table html_nodes
#' @examples
#' if (!is_travis()) {
#' library(httr)
#' library(rvest) 
#' library(gcite)
#' url = "https://scholar.google.com/citations?user=T9eqZgMAAAAJ"
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' ind = gcite_citation_index(url)
#' doc = content(httr::GET(url))
#' ind = gcite_citation_index(doc)
#' ind_nodes = rvest::html_nodes(doc, "#gsc_rsb_st")[[1]]
#' ind = gcite_citation_index(ind_nodes)
#' }
gcite_citation_index <- function(doc, ...){
  UseMethod("gcite_citation_index")
}

#' @rdname gcite_citation_index
#' @export
gcite_citation_index.xml_node = function(doc, ...) {
  rvest::html_table(doc)
}

#' @rdname gcite_citation_index
#' @export
gcite_citation_index.xml_document = function(doc, ...) {
  doc = rvest::html_nodes(doc, "#gsc_rsb_st")[[1]]
  gcite_citation_index(doc, ...)
}

#' @rdname gcite_citation_index
#' @export
gcite_citation_index.character = function(doc, ...) {
  res = httr::GET(url = doc)
  stop_for_status(res)
  doc = httr::content(res)
  gcite_citation_index(doc, ...)
}


