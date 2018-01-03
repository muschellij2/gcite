
#' @title Google Citations Information
#' @description Wraps getting the information from Google Citations and
#' plotting the wordcloud
#' @param author author name separated by spaces
#' @param user user ID for google Citations
#' @param plot_wordcloud should the wordcloud be plotted
#' @param sleeptime = time in seconds between http requests, to avoid Google Scholar rate limit
#' @param ... additional options passed to \code{\link{gcite_wordcloud}}
#' @return List from either \code{\link{gcite_user_info}}
#' or \code{\link{gcite_author_info}}
#' @export
gcite = function(author,
                 user,
                 plot_wordcloud = TRUE,
                 sleeptime = 0,
                 ...) {
  if (missing(user) && missing(author)) {
    stop("Must specify either the user or author")
  }
  if (!missing(user)) {
    L = gcite_user_info(user = user)
  }
  if (!missing(author)) {
    L = gcite_author_info(author = author)
  }
  paper_df = L$paper_df
  if (plot_wordcloud) {
    gcite_wordcloud(paper_df = paper_df, ...)
  }
  return(L)
}
