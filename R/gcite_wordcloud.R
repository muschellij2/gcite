#' @title Wordcloud of Google Citations Information
#' @description Simple wrapper for \code{\link{author_cloud}}
#' and  \code{\link{title_cloud}}
#' @param paper_df A \code{data.frame} with columns of authors and titles
#' @param author_args Arguments to pass to \code{\link{author_cloud}}
#' @param title_args Arguments to pass to \code{\link{title_cloud}}
#' @param warn should warnings be printed from wordcloud?
#' @return NULL
#' @export
#' @importFrom graphics par
gcite_wordcloud = function(paper_df, 
                           author_args = list(),
                           title_args = list(),
                           warn = FALSE) {
  opar = par(no.readonly = TRUE)
  on.exit({
    par(opar)
  })
  par(mfrow = c(1,2))
  author_args$authors = paper_df$authors
  title_args$titles = paper_df$title
  fun = suppressWarnings
  if (warn) {
    fun = identity
  }
  fun({
    do.call(author_cloud, args = author_args)
  })
  fun({
    do.call(title_cloud, args = title_args)
  })
  return(invisible(NULL))
}

