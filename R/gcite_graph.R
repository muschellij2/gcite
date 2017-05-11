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
  gcite_graph(citations, ...)
}

#' @rdname gcite_graph
#' @export
gcite_graph.xml_document = function(citations, ...) {
  citations = rvest::html_node(citations, css = "#gsc_graph_bars")
  gcite_graph(citations, ...)
}

#' @rdname gcite_graph
#' @export
gcite_graph.character = function(citations, ...) {
  res = httr::GET(url = citations)
  stop_for_status(res)
  citations = httr::content(res)
  gcite_graph(citations, ...)
}


#' @rdname gcite_graph
#' @export
gcite_graph.default = function(citations, ...) {
  cn = names(citations) 
  # spans = cn %in% "span"
  a_s = cn %in% "a"
  # years = unlist(citations[spans])
  info = lapply(citations[a_s], function(x) {
    cc = x$span[[1]]
    if (is.null(cc)) {
      cc = 0
    }
    cc = as.numeric(cc)
    href = attributes(x)$href
    y = parse_url(href)$query[c("as_ylo", "as_yhi")]
    y = unlist(unique(y))
    if (length(y) == 0) {
      y = NA
    }
    y = as.numeric(y)
    data.frame(year = y, n_citations = cc,stringsAsFactors = FALSE)
  })
  info = as.data.frame(data.table::rbindlist(info, fill = TRUE))
  
  # 
  # cites = unlist(citations[a_s])
  # years = as.numeric(years)
  # cites = as.numeric(cites)
  # if (length(cites) != length(years)) {
  #   warning("Getting yearly citations may not work correctly")
  # }
  # cites = cbind(year = years, n_citations = cites)
  info
}

