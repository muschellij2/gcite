#' @title Parse Google Citations Over Time
#' @description Parses a google citations over time from the main
#' Citation page
#'
#' @param doc A xml_document or the url for the main page
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return A matrix of citations
#' @export
#' @importFrom xml2 as_list
#' @importFrom rvest html_node
#' @importFrom httr GET content
#' @examples 
#' library(httr)
#' library(rvest) 
#' url = "https://scholar.google.com/citations?user=T9eqZgMAAAAJ"
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' if (!is_travis() & !is_cran()) {
#' #' ind = gcite_cite_over_time(url)
#' doc = content(httr::GET(url))
#' ind = gcite_cite_over_time(doc)
#' ind_nodes = rvest::html_nodes(doc, ".gsc_md_hist_b")
#' ind = gcite_cite_over_time(ind_nodes)
#' }
gcite_cite_over_time <- function(doc, ...){
  UseMethod("gcite_cite_over_time")
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.xml_node = function(doc, ...) {
  gcite_cite_over_time.default(doc)
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.xml_document = function(doc, ...) {
  # doc = rvest::html_node(doc, css = "#gsc_g")
  doc = rvest::html_node(doc, css = ".gsc_md_hist_b")
  # doc = rvest::html_node(doc, css = ".gsc_md_hist_w")
  gcite_cite_over_time(doc)
}

#' @rdname gcite_cite_over_time
#' @export
gcite_cite_over_time.character = function(doc, ...) {
  res = httr::GET(url = doc, ...)
  stop_for_status(res)
  doc = httr::content(res)
  gcite_cite_over_time(doc)
}

#' @rdname gcite_cite_over_time
#' @importFrom rvest html_attr
#' @export
gcite_cite_over_time.default = function(doc, ...) {
  tdoc = rvest::html_nodes(doc, css = ".gsc_g_t")
  tdoc = html_text(tdoc)
  tdoc = as.numeric(tdoc)
  adoc = rvest::html_nodes(doc, css = ".gsc_g_a")
  adoc = html_text(adoc)  
  adoc = as.numeric(adoc)
  
  # adapted from 
  # https://github.com/jkeirstead/scholar/pull/30/files
  if (length(tdoc) > length(adoc)) {
    # Some years don't have citations.
    # We need to match the citation counts and years
    # <a href="javascript:void(0)" class="gsc_g_a" 
    # style="left:8px;height:5px;z-index:9">\n  
    # <span class="gsc_g_al">2</span>\n</a>
    style_tags = rvest::html_nodes(doc, css = ".gsc_g_a")
    style_tags = html_attr(style_tags, "style")
    # these z indices seem to be the indices starting with the last year
    zindices = gsub('.*z-index:([0-9]+).*', "\\1", style_tags, )
    zindices = as.integer(zindices)
    # empty vector of 0s
    allvals = integer(length = length(tdoc))
    # fill in
    allvals[zindices] = adoc
    # and then reverse
    adoc = rev(allvals)
  }
  
  full_cite_over_time = data.frame(
    year = as.numeric(tdoc), 
    n_citations = as.numeric(adoc),
    stringsAsFactors = FALSE)
  return(full_cite_over_time)
}


