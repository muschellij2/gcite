#' @title Parse Google Citation Graph
#' @description Parses a google citation bar graph from html
#'
#' @param citations A list of nodes or xml_node
#' @param ... not currently used
#'
#' @return A matrix of citations and years
#' @export
#' @importFrom xml2 as_list
gcite_graph <- function(citations, ...){
  UseMethod("gcite_graph")
}

#' @rdname gcite_graph
#' @export
gcite_graph.xml_node = function(citations, ...) {
  citations = xml2::as_list(citations)
  gcite_graph.default(citations, ...)
}

#' @rdname gcite_graph
#' @export
gcite_graph.list = function(citations, ...) {
  gcite_graph.default(citations, ...)
}

#' @rdname gcite_graph
#' @export
gcite_graph.default = function(citations, ...) {
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
  cites = cbind(years, cites)
}