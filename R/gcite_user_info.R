#' @title Getting User Information of papers
#' @description Loops through pages for all information on Google Citations
#' @param user user ID for google Citations
#' @param pagesize Size of pages, max 100, passed to \code{\link{gcite_url}}
#' @param verbose Print diagnostic messages
#' @param ... Not used
#'
#' @return A list of citations, citation indices, and a 
#' \code{data.frame} of authors, journal, and citations, and a 
#' \code{data.frame} of the links to all paper URLs.
#' @export
#'
#' @examples
#' df = gcite_user_info(user = "T9eqZgMAAAAJ")
gcite_author_info = function(user, pagesize = 100, verbose = TRUE, ...) {
  url = paste0("https://scholar.google.com/citations?user=", user)
  
  #############################################
  # Getting initial URL
  #############################################  
  url = gcite_url(url, pagesize = pagesize, cstart = 0)
  res = httr::GET(url = url)
  stop_for_status(res)
  doc = httr::content(res)
  
  if (verbose) {
    message("Getting Citation Index table")
  }
  cite_ind = gcite_citation_index(doc)
  
  if (verbose) {
    message("Getting Overall citations over time")
  }  
  overall_cite = gcite_main_graph(doc)
  
  if (verbose) {
    message("Getting first set of papers")
  }  
  papers = gcite_papers(doc)
  
  if (verbose) {
    message("Getting rest of papers")
  }  
  cstart = pagesize + 1
  
  #######################
  # looping over papers
  all_papers = papers
  while (!is.null(papers)) {
    url = gcite_url(url, pagesize = pagesize, cstart = cstart)
    papers = gcite_papers(url)
    all_papers = rbind(all_papers, papers)
    cstart = pagesize + cstart
  }
  
  urls = all_papers$title_link
  paper_info = pbapply::pblapply(urls, gcite_citation_page)
  paper_df = data.table::rbindlist(paper_info, fill = TRUE)
  paper_df = as.data.frame(paper_df)
  L = list(citation_indices = cite_ind,
           overall_citations = overall_cite,
           all_papers = all_papers,
           df = paper_df
           )
  return(L)
}


#' @title Getting User Information from name
#' @description Calls \code{\link{gcite_user_info}} after getting the user
#' identifier
#' @param author author name separated by spaces
#' @param verbose Verbose diagnostic printing
#' @param ask If multiple authors are found, should a menu be given
#' @param ... arguments passed to \code{\link{gcite_user_info}}
#'
#' @return A list of citations, citation indices, and a 
#' \code{data.frame} of authors, journal, and citations, and a 
#' \code{data.frame} of the links to all paper URLs.
#' @export
#'
#' @examples
#' df = gcite_author_info(author = "John Muschelli")
gcite_author_info = function(author, 
                           verbose = TRUE, 
                           ask = TRUE, ...) {
  user = gcite_username(author = author, verbose = verbose, ask = ask)
  
  return(gcite_author_info(user = user, ...))
}