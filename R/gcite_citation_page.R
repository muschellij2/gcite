#' @title Parse Google Citation Index
#' @description Parses a google citation indices (h-index, etc.) from main page
#'
#' @param doc A xml_document or the url for the main page
#' @param title title of the article 
#' @param force If passing a URL and there is a failure, should the 
#' program return \code{NULL}?
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return A matrix of indices
#' @export
#' @importFrom rvest html_table html_nodes
#' @importFrom httr stop_for_status status_code warn_for_status
#' @importFrom stats reshape
#' @examples 
#' library(httr)
#' library(rvest)
#' url = paste0("https://scholar.google.com/citations?view_op=view_citation&", 
#' "hl=en&oe=ASCII&user=T9eqZgMAAAAJ&pagesize=100&", 
#' "citation_for_view=T9eqZgMAAAAJ:W7OEmFMy1HYC")
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' if (!is_travis() & !is_cran()) {
#' ind = gcite_citation_page(url)
#' doc = content(httr::GET(url))
#' ind = gcite_citation_page(doc)
#' ind_nodes = html_nodes(doc, "#gsc_vcd_table div")
#' ind_nodes = html_nodes(ind_nodes, xpath = '//div[@class = "gs_scl"]')  
#' ind = gcite_citation_page(ind_nodes)
#' }
gcite_citation_page <- function(doc, title = NULL, 
                                force = FALSE, ...){
  UseMethod("gcite_citation_page")
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.xml_nodeset = function(doc, title = NULL, 
                                           force = FALSE, ...) {
  gcite_citation_page.default(doc, title = title)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.xml_document = function(doc, title = NULL,
                                            force = FALSE, ...) {
  if (is.null(title)) {
    title = html_nodes(doc, "#gsc_vcd_title")
    title = html_text(title)
    if (length(title) == 0) {
      title = html_nodes(doc, ".gsc_oci_title_link")
      title = html_text(title)
    }
  }
  # doc = html_nodes(doc, "#gsc_table div")
  doc = html_nodes(doc, "#gsc_oci_table div")
  doc = html_nodes(doc, xpath = '//div[@class = "gs_scl"]')  
  gcite_citation_page(doc, title = title)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.character = function(doc, title = NULL, 
                                         force = FALSE, ...) {
  res = httr::GET(url = doc, ...)
  if (force) {
    if (httr::status_code(res) > 300) {
      warn_for_status(res)
      return(NULL)
    }
  } else {
    stop_for_status(res)
  }
  doc = httr::content(res)
  gcite_citation_page(doc, title = title)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.list = function(doc, title = NULL, 
                                    force = FALSE, ...) {
  lapply(doc, gcite_citation_page, title = title, force = force, ...)
}

#' @rdname gcite_citation_page
#' @export
gcite_citation_page.default = function(doc, title = NULL,
                                       force = FALSE, ...) {
  
  # fields = html_nodes(doc, xpath = '//div[@class = "gsc_field"]')
  # fields = html_nodes(doc, xpath = '//div[@class = "gsc_vcd_field"]')
  fields = html_nodes(doc, xpath = '//div[@class = "gsc_oci_field"]')
  fields = html_text(fields)
  
  # vals = html_nodes(doc, xpath = '//div[@class = "gsc_value"]')
  # vals = html_nodes(doc, xpath = '//div[@class = "gsc_vcd_value"]')
  vals = html_nodes(doc, xpath = '//div[@class = "gsc_oci_value"]')
  vals = html_text(vals)
  df = data.frame(field = fields, value = vals, stringsAsFactors = FALSE)
  keep_fields = c("authors", "publication date", 
                  "journal", "volume", "issue", 
                  "pages", "publisher", "description",
                  "total citations")
  df$field = tolower(df$field)
  is_patent = any(grepl("patent", df$field))
  df$field[df$field %in% "inventors"] = "authors"
  df$field[df$field %in% "patent office"] = "journal"
  df$field[df$field %in% "patent number"] = "volume"
  df$field[df$field %in% "application number"] = "issue"
  
  #############################
  # need different way to get total citations
  #############################  
  cites = html_nodes(doc, xpath = '//a[@class = "gsc_oms_link"]')
  cites = html_text(cites)
  cites = cites[ grep("cited", tolower(cites))]
  cites = trimws(sub("Cited by", "", cites))
  
  df$value[ df$field %in% "total citations" ] = cites
  df = df[ df$field %in% 
             keep_fields,]
  
  
  if (nrow(df) > 1) {
    df$id = 1
    wide = reshape(df, direction = "wide", idvar = "id", timevar = "field")
    colnames(wide) = gsub("^value[.]", "", colnames(wide))
    wide$patent = is_patent
    wide$id = NULL
  } else {
    wide = t(rep(NA, length(keep_fields)))
    colnames(wide) = keep_fields
    wide$patent = FALSE
    wide = data.frame(wide, stringsAsFactors = FALSE)
  }
  wide$title = title
  
  # citations = rvest::html_node(doc, css = "#gsc_vcd_graph_bars")
  citations = rvest::html_node(doc, css = "#gsc_oci_graph_bars")
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
