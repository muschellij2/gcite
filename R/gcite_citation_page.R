#' @title Parse Google Citation Index
#' @description Parses a google citation indices (h-index, etc.) from main page
#'
#' @param doc A xml_document or the url for the main page
#' @param ... not currently used
#'
#' @return A matrix of indices
#' @export
#' @importFrom rvest html_table html_nodes
#' @importFrom httr stop_for_status
#' @importFrom stats reshape
#' @examples 
#' library(httr)
#' library(rvest)
#' url = paste0("https://scholar.google.com/citations?view_op=view_citation&", 
#' "hl=en&oe=ASCII&user=T9eqZgMAAAAJ&pagesize=100&", 
#' "citation_for_view=T9eqZgMAAAAJ:W7OEmFMy1HYC")
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' ind = gcite_citation_page(url)
#' doc = content(httr::GET(url))
#' ind = gcite_citation_page(doc)
#' ind_nodes = html_nodes(doc, "#gsc_table div")
#' ind_nodes = html_nodes(ind_nodes, xpath = '//div[@class = "gs_scl"]')  
#' ind = gcite_citation_page(ind_nodes)
gcite_citation_page <- function(doc, ...){
  UseMethod("gcite_citation_page")
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.xml_nodeset = function(doc, ...) {
  gcite_citation_page.default(doc, ...)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.xml_document = function(doc, ...) {
  doc = html_nodes(doc, "#gsc_table div")
  doc = html_nodes(doc, xpath = '//div[@class = "gs_scl"]')  
  gcite_citation_page(doc, ...)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.character = function(doc, ...) {
  res = httr::GET(url = doc)
  stop_for_status(res)
  doc = httr::content(res)
  gcite_citation_page(doc, ...)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.list = function(doc, ...) {
  lapply(doc, gcite_citation_page, ...)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.default = function(doc, ...) {
  
    fields = html_nodes(doc, xpath = '//div[@class = "gsc_field"]')
    fields = html_text(fields)
    
    vals = html_nodes(doc, xpath = '//div[@class = "gsc_value"]')
    vals = html_text(vals)
    df = data.frame(field = fields, value = vals, stringsAsFactors = FALSE)
    df$field = tolower(df$field)
    df = df[ df$field %in% 
               c("authors", "publication date", "journal", "volume", "issue", 
                 "pages", "publisher", "description"),]
    
    
    
    
    df$id = 1
    wide = reshape(df, direction = "wide", idvar = "id", timevar = "field")
    colnames(wide) = gsub("^value[.]", "", colnames(wide))
    wide$id = NULL
    
    citations = rvest::html_node(doc, css = "#gsc_graph_bars")
    citations = citations[!is.na(citations)]
    if ( length(citations) > 0) {
      citations = citations[[1]]
      citations = gcite_graph(citations)    
      if ( nrow(citations) > 0) {
        citations = data.frame(citations)
        citations$id = 1
        citations = reshape(citations, direction = "wide", 
                            idvar = "id", timevar = "year")
        colnames(citations) = gsub("^n_citations[.]", "", colnames(citations))
        citations$id = NULL
        wide = cbind(wide, citations)
      }
    }
    
    return(wide)
  }
  