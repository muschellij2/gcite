#' Get Paper Data Frame from Title URLs
#'
#' @param urls A character vector of urls, from 
#' \code{all_papers$title_link}
#' @param verbose Print diagnostic messages
#' @param force If passing a URL and there is a failure, should the 
#' program return \code{NULL}, passed to \code{\link{gcite_citation_page}}
#' @param sleeptime time in seconds between http requests, 
#' to avoid Google Scholar rate limit 
#' @param ... Additional arguments passed to \code{\link[httr]{GET}}
#'
#' @return A \code{data.frame} of authors, journal, and citations
#' @export
#' @examples 
#' if (!is_travis() & !is_cran()) {
#' L = gcite_user_info(user = "uERvKpYAAAAJ", 
#' read_citations = FALSE)
#' urls = L$all_papers$title_link
#' paper_df = gcite_paper_df(urls = urls, force = TRUE)
#' } 
gcite_paper_df = function(
  urls, 
  verbose = TRUE, 
  force = FALSE,
  sleeptime = 0,
  ...) {  
  
  if (verbose) {
    message("Reading citation pages")
  }  
  paper_info = pbapply::pblapply(
    urls, 
    function(x) {
      Sys.sleep(sleeptime)
      gcite_citation_page(x,
                          force = force,
                          ... = ...)
    }
  )
  paper_df = data.table::rbindlist(paper_info, fill = TRUE)
  paper_df = as.data.frame(paper_df)
  cn = colnames(paper_df)
  suppressWarnings({
    num_cn = as.numeric(cn)
  })
  cn = c(cn[is.na(num_cn)], sort(num_cn[ !is.na(num_cn)]))
  paper_df = paper_df[, cn]
  return(paper_df)
}
