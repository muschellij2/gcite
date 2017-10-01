#' @title Make Wordcloud of Titles from Papers
#' @description Takes a vector of titles and then creates a frequency table
#' of those words and plots a wordcloud
#' 
#' @param titles Vector of titles of papers
#' @param addstopwords Additional words to remove from wordcloud
#' @param ... additional options passed to \code{\link{gcite_wordcloud_spec}}
#'
#' @return A \code{data.frame} of the words and the frequencies of the
#' title words
#' @export
#'
#' @examples
#' \dontrun{
#' L = gcite_author_info("John Muschelli")
#' paper_df = L$paper_df
#' titles = paper_df$title
#' title_cloud(titles)
#' }
#' @importFrom tm TermDocumentMatrix VectorSource Corpus
#' @importFrom tm content_transformer removePunctuation Corpus removeWords
#' @importFrom tm stopwords tm_map
#' @importFrom wordcloud wordcloud
title_cloud = function(
  titles,
  addstopwords = gcite_stopwords(),
  ...) {
  freq_df = title_word_frequency(titles, 
                                 addstopwords = addstopwords)
  
  gcite_wordcloud_spec(    
    words = freq_df$word,
    freq = freq_df$freq,
    ...)
  return(freq_df)
}

#' @rdname title_cloud
#' @export
paper_cloud = function(...){
  title_cloud(...)
}

#' @rdname title_cloud
#' @export
title_word_frequency = function(titles,
                                addstopwords = NULL) {
  corpus = Corpus(VectorSource(titles))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  if (!is.null(addstopwords)) {
    corpus <- tm_map(corpus, removeWords, addstopwords)
  }
  
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  return(d)
}

