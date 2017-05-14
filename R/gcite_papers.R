#' @title Parse Google Citation Index
#' @description Parses a google citation indices (h-index, etc.) from main page
#'
#' @param doc A xml_document or the url for the main page
#' @param ... not currently used
#'
#' @return A matrix of indices
#' @export
#' @importFrom rvest html_table html_nodes
#' @importFrom stats reshape
#' @examples 
#' \dontrun{
#' library(httr)
#' library(rvest) 
#' url = "https://scholar.google.com/citations?user=T9eqZgMAAAAJ"
#' url = gcite_url(url = url, pagesize = 10, cstart = 0) 
#' ind = gcite_papers(url)
#' doc = content(httr::GET(url))
#' ind = gcite_papers(doc)
#' ind_nodes = rvest::html_nodes(doc, "#gsc_a_b")
#' ind = gcite_papers(ind_nodes)
#' }
gcite_papers <- function(doc, ...){
  UseMethod("gcite_papers")
}

#' @rdname gcite_papers
#' @export
gcite_papers.xml_nodeset = function(doc, ...) {
  doc = as_list(doc)[[1]]
  gcite_papers(doc, ...)
}

#' @rdname gcite_papers
#' @export
gcite_papers.xml_document = function(doc, ...) {
  doc = rvest::html_nodes(doc, css = "#gsc_a_b")
  gcite_papers(doc, ...)
}

#' @rdname gcite_papers
#' @export
gcite_papers.character = function(doc, ...) {
  res = httr::GET(url = doc)
  stop_for_status(res)
  doc = httr::content(res)
  gcite_papers(doc, ...)
}

#' @rdname gcite_papers
#' @export
gcite_papers.default = function(doc, ...) {
  
  tab = doc
  tab = lapply(tab, function(x) {
    x = unlist(x, recursive = FALSE)
    names(x) = gsub("^td[.]", "", names(x))
    x
  })
  parse_a = function(a) {
    a = cbind(value = a, 
              href = attr(a, "href"),
              class = attr(a, ".class"))
    a = data.frame(a, stringsAsFactors = FALSE)
  }
  fake_a_df = data.frame(title = NA, 
                         title_link = NA, n_citations = NA,
                         n_citations_link = NA)
  all_a = lapply(tab, function(x) {
    # print(x)
    nx = names(x)
    a = nx == "a"
    a = lapply(x[a], parse_a)
    a = do.call("rbind", a)
    if (NROW(a) == 0) {
      return(fake_a_df)
    }
    a$id = 1
    a$value = trimws(a$value)
    a$href = trimws(a$href)
    a = reshape(a, direction = "wide", idvar = "id", 
                timevar = "class", v.names = c("value", "href"))
    cn = colnames(a)
    cn[ cn == "value.gsc_a_at"] = "title"
    cn[ cn == "value.gsc_a_ac"] = "n_citations"
    cn[ cn == "href.gsc_a_at"] = "title_link"
    cn[ cn == "href.gsc_a_ac"] = "n_citations_link"
    colnames(a) = cn
    a$id = NULL
    a = a[, c("title", "title_link", "n_citations", "n_citations_link")]
    
    a
  })
  all_a = do.call("rbind", all_a)
  rownames(all_a) = NULL
  
  
  all_a$n_citations = as.numeric(all_a$n_citations)
  
  all_a$n_citations_link[ all_a$n_citations_link == "" ] = NA
  all_a$title_link[ all_a$title_link == "" ] = NA
  all_a$title_link[ !is.na(all_a$title_link)] = 
    paste0("https://scholar.google.com",
           all_a$title_link[ !is.na(all_a$title_link)])
  if (all(is.na(all_a))) {
    return(NULL)
  }
  all_a$n_citations[ is.na(all_a$n_citations)] = 0  
  return(all_a)
}

