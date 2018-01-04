#' @title Parse Google Citation Graph
#' @description Parses a google citation bar graph from html
#'
#' @param citations A list of nodes or xml_node
#' @param ... arguments passed to \code{\link{GET}}
#'
#' @return A matrix of citations and years
#' @export
#' @importFrom xml2 as_list
gcite_main_graph <- function(citations, ...){
  UseMethod("gcite_main_graph")
}

#' @rdname gcite_main_graph
#' @export
gcite_main_graph.xml_document = function(citations, ...) {
  citations = html_node(citations, css = "#gsc_g") 
  citations = as_list(citations)
  citations = unlist(citations, recursive = FALSE)
  names(citations) = gsub("^div[.]", "", names(citations))
  gcite_main_graph.default(citations, ...)
}

#' @rdname gcite_main_graph
#' @export
gcite_main_graph.character = function(citations, ...) {
  res = httr::GET(url = citations, ...)
  stop_for_status(res)
  citations = httr::content(res)
  gcite_main_graph(citations)
}


#' @rdname gcite_main_graph
#' @export
gcite_main_graph.default = function(citations, ...) {
  cn = names(citations) 
  spans = cn %in% "span"
  a_s = cn %in% "a"
  years = unlist(citations[spans])
  cites = unlist(citations[a_s])
  years = as.numeric(years)
  cites = as.numeric(cites)
  if (length(cites) != length(years)) {
    warning("Getting yearly citations may not work correctly")
  }
  cites = cbind(year = years, n_citations = cites)
  return(cites)
}