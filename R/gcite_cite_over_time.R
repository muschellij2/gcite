#' @title Parse Google Citations Over Time
#' @description Parses a google citations over time from the main
#' Citation page
#'
#' @param doc A xml_document or the url for the main page
#' @param ... not currently used
#'
#' @return A matrix of citations
#' @export
#' @importFrom xml2 as_list
#' @importFrom rvest html_node
#' @importFrom httr GET content
#' @examples 
#' url = "https://scholar.google.com/citations?user=T9eqZgMAAAAJ"
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' ind = gcite_cite_over_time(url)
#' doc = content(httr::GET(url))
#' ind = gcite_cite_over_time(doc)
#' ind_nodes = rvest::html_nodes(doc, "#gsc_rsb_st")[[1]]
#' ind = gcite_cite_over_time(ind_nodes)
gcite_cite_over_time <- function(doc, ...){
  UseMethod("gcite_cite_over_time")
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.xml_node = function(doc, ...) {
  doc = xml2::as_list(doc)  
  gcite_cite_over_time(doc, ...)
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.xml_document = function(doc, ...) {
  doc = rvest::html_node(doc, css = "#gsc_g")
  doc = xml2::as_list(doc)  
  gcite_cite_over_time(doc, ...)
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.character = function(doc, ...) {
  res = httr::GET(url = doc)
  doc = httr::content(res)
  gcite_cite_over_time(doc, ...)
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.default = function(doc, ...) {
  full_cite_over_time = unlist(doc, recursive = FALSE)
  cn = names(full_cite_over_time)
  cn = gsub("^div[.]", "", cn)
  names(full_cite_over_time) = cn
  full_cite_over_time = gcite_graph(full_cite_over_time)
  return(full_cite_over_time)
}


