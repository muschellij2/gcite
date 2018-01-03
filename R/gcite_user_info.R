#' @title Getting User Information of papers
#' @description Loops through pages for all information on Google Citations
#' @param user user ID for google Citations
#' @param pagesize Size of pages, max 100, passed to \code{\link{gcite_url}}
#' @param verbose Print diagnostic messages
#' @param secure use https vs. http
#'
#' @param ... Not used
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
  ...) {
  url = paste0("http", ifelse(secure, "s", ""),
               "://scholar.google.com/citations?user=", user)

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
  # overall_cite = gcite_main_graph(doc)
  overall_cite = gcite_cite_over_time(doc)

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
    Sys.sleep(10)
    all_papers = rbind(all_papers, papers)
    cstart = pagesize + cstart
  }

  if (verbose) {
    message("Reading citation pages")
  }
  urls = all_papers$title_link
  paper_info = pbapply::pblapply(urls, gcite_citation_page)
  paper_df = data.table::rbindlist(paper_info, fill = TRUE)
  paper_df = as.data.frame(paper_df)
  cn = colnames(paper_df)
  suppressWarnings({
    num_cn = as.numeric(cn)
  })
  cn = c(cn[is.na(num_cn)], sort(num_cn[ !is.na(num_cn)]))
  paper_df = paper_df[, cn]

  # paper_df$title =
  L = list(citation_indices = cite_ind,
           overall_citations = overall_cite,
           all_papers = all_papers,
           paper_df = paper_df,
           user = user
  )
  return(L)
}
