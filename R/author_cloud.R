#' @title Make Wordcloud of authors from Papers
#' @description Takes a vector of authors and then creates a frequency table
#' of those words and plots a wordcloud
#' 
#' @param authors Vector of authors of papers
#' @param addstopwords Additional words to remove from wordcloud
#' @param author_pattern regular expression for patterns to exclude from 
#' individual authors 
#' @param split split author names (default \code{","}), passed to
#' \code{\link{strsplit}}
#' @param verbose Print diagnostic messages
#' @param colors color words from least to most frequent.  Passed to 
#' \code{\link{gcite_wordcloud_spec}}
#' @param ... additional options passed to \code{\link{gcite_wordcloud_spec}}
#'
#' @return A \code{data.frame} of the words and the frequencies of the
#' authors
#' @export
#'
#' @examples
#' \dontrun{
#' L = gcite_author_info("John Muschelli")
#' paper_df = L$paper_df
#' authors = paper_df$authors
#' author_cloud(authors)
#' }
#' @importFrom tm TermDocumentMatrix VectorSource Corpus
#' @importFrom tm content_transformer removePunctuation Corpus removeWords
#' @importFrom tm stopwords tm_map
#' @importFrom wordcloud wordcloud
author_cloud = function(
  authors,
  addstopwords = gcite_stopwords(),
  author_pattern = NULL,
  split = ",",
  verbose=TRUE,
  colors = c("#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B"),
  ...) {
  freq_df = author_frequency(authors, 
                             addstopwords = addstopwords,
                             author_pattern = author_pattern,
                             split = split,
                             verbose = verbose)
  
  gcite_wordcloud_spec(    
    words = freq_df$word,
    freq = freq_df$freq,
    colors = colors,
    ...)
  return(freq_df)
}

#' @rdname author_cloud
#' @export
author_frequency = function(authors,
                            author_pattern = NULL,
                            split = ",",
                            addstopwords = gcite_stopwords(),
                            verbose=TRUE) {
  if (verbose) {
    if (!is.null(author_pattern)) {
      message(paste("Author Pattern:", author_pattern))  
    }
  }
  authors = trimws(authors)
  authors = tolower(authors)
  tmp = strsplit(authors, split = split)
  tmp <- lapply(tmp, trimws)
  if (!is.null(author_pattern)) {
    author_pattern = tolower(author_pattern)
    tmp <- lapply(tmp, function(x) {
      keep = !grepl(x, pattern = author_pattern)
      x = x[keep]
      x
    })
  }
  
  last = function(x) {
    x[length(x)]
  }
  #out <- sapply(tmp, function(x) return(x[length(x)]))
  out = sapply(tmp, function(x) {
    x = strsplit(x, " ")
    x = sapply(x, last)
  })
  
  
  out = unlist(out)
  
  out = out[ !(out %in% c("...", tolower(addstopwords))) ]
  out = removePunctuation(out)
  
  out = table(out)
  d = as.data.frame(out)
  colnames(d) = c("word", "freq") 
  d = d[order(d$freq, decreasing = TRUE),]
  
  return(d)
}

