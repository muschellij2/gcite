#' @title Getting User Information of papers
#' @description Loops through pages for all information on Google Citations
#' @param user user ID for google Citations
#' @param pagesize Size of pages, max 100, passed to \code{\link{gcite_url}}
#' @param verbose Print diagnostic messages
#' @param secure use https vs. http
#' @param force If passing a URL and there is a failure, should the 
#' program return \code{NULL}, passed to \code{\link{gcite_citation_page}}
#' @param read_citations Should all citation pages be read?
#' @param ... Additional arguments passed to \code{\link{GET}}
#'
#' @return A list of citations, citation indices, and a 
#' \code{data.frame} of authors, journal, and citations, and a 
#' \code{data.frame} of the links to all paper URLs and the character
#' string of the user name.
#' @export
#'
#' @examples
#' if (!is_travis()) {
#' df = gcite_user_info(user = "T9eqZgMAAAAJ")
#' }
gcite_user_info = function(
  user, pagesize = 100, 
  verbose = TRUE, 
  secure = TRUE,
  force = FALSE,
  read_citations = TRUE,
  ...) {
  url = paste0("http", ifelse(secure, "s", ""), 
               "://scholar.google.com/citations?user=", user)
  
  #############################################
  # Getting initial URL
  #############################################  
  url = gcite_url(url, pagesize = pagesize, cstart = 0)
  res = httr::GET(url = url, ...)
  stop_for_status(res)
  doc = httr::content(res)
  
  if (verbose) {
    message("Getting Citation Index table")
  }
  cite_ind = gcite_citation_index(doc)
  
  if (verbose) {
    message("Getting Overall citations over time")
  }  
  # overall_cite = gcite_main_graph(doc)
  overall_cite = gcite_cite_over_time(doc)
  
  if (verbose) {
    message("Getting first set of papers")
  }  
  papers = gcite_papers(doc, ...)
  
  if (verbose) {
    message("Getting rest of papers")
  }  
  cstart = pagesize + 1
  
  #######################
  # looping over papers
  all_papers = papers
  while (!is.null(papers)) {
    url = gcite_url(url, pagesize = pagesize, cstart = cstart)
    papers = gcite_papers(url, ...)
    all_papers = rbind(all_papers, papers)
    cstart = pagesize + cstart
  }
  
  paper_df = NULL
  if (read_citations) {
    paper_df = gcite_paper_df(
      urls = all_papers$title_link,
      verbose = verbose, 
      force = force, 
      ... = ...)
    # if (verbose) {
    #   message("Reading citation pages")
    # }  
    # urls = all_papers$title_link
    # paper_info = pbapply::pblapply(
    #   urls, 
    #   gcite_citation_page,
    #   force = force,
    #   ... = ...)
    # paper_df = data.table::rbindlist(paper_info, fill = TRUE)
    # paper_df = as.data.frame(paper_df)
    # cn = colnames(paper_df)
    # suppressWarnings({
    #   num_cn = as.numeric(cn)
    # })
    # cn = c(cn[is.na(num_cn)], sort(num_cn[ !is.na(num_cn)]))
    # paper_df = paper_df[, cn]
  }
  # paper_df$title =
  L = list(citation_indices = cite_ind,
           overall_citations = overall_cite,
           all_papers = all_papers,
           user = user
  )
  L$paper_df = paper_df
  return(L)
}

