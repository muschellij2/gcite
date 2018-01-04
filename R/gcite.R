
#' @title Google Citations Information
#' @description Wraps getting the information from Google Citations and
#' plotting the wordcloud
#' @param author author name separated by spaces
#' @param user user ID for google Citations
#' @param plot_wordcloud should the wordcloud be plotted
#' @param author_args Arguments to pass to \code{\link{author_cloud}}
#' @param title_args Arguments to pass to \code{\link{title_cloud}}
#' @param warn should warnings be printed from wordcloud?
#' @param force If passing a URL and there is a failure, should the
#' program return \code{NULL}, passed to \code{\link{gcite_citation_page}}
#' @param sleeptime time in seconds between http requests,
#' to avoid Google Scholar rate limit 
#'
#' @param ... additional options passed to \code{\link{gcite_user_info}}
#' and therefore \code{\link{GET}}
#'
#' @return List from either \code{\link{gcite_user_info}}
#' or \code{\link{gcite_author_info}}
#' @export
gcite = function(author,
                 user,
                 plot_wordcloud = TRUE,
                 author_args = list(),
                 title_args = list(),
                 warn = FALSE,
                 force = FALSE,
                 sleeptime = 0,
                 ...) {
  if (missing(user) && missing(author)) {
    stop("Must specify either the user or author")
  }
  if (!missing(user)) {
    L = gcite_user_info(
      user = user, force = force,
      sleeptime = sleeptime, ...)
  } else {
    if (!missing(author)) {
      L = gcite_author_info(
        author = author, force = force,
        sleeptime = sleeptime,
        ...)
    } else {
      stop("User and author not specified!")
    }
  }
  paper_df = L$paper_df
  if (plot_wordcloud) {
    if (is.null(paper_df)) {
      warning(paste0("Combination of arguments did not result in ",
                     "paper_df output, ",
                     "may be rate limited or read_citations = FALSE"))
    } else {
      gcite_wordcloud(
        paper_df = paper_df,
        author_args = author_args,
        title_args = title_args,
        warn = warn)
    }
  }
  return(L)
}
