
#' @title gcite Wordcloud default
#' @description Simple wrapper for \code{\link{wordcloud}} with 
#' different defaults
#' 
#' @param words words to be plotted
#' @param freq the frequency of those words
#' @param min.freq words with frequency below min.freq will not be plotted
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param random.order plot words in random order. If false, they will be plotted in decreasing frequency
#' @param colors color words from least to most frequent
#' @param vfont passed to text for the font
#' @param ... additional options passed to \code{\link{wordcloud}}
#' 
#' @return Nothing
#' @export
#' @importFrom wordcloud wordcloud
gcite_wordcloud_spec = function(
  words,
  freq,
  min.freq = 1,
  max.words = Inf,
  random.order = FALSE,
  colors = c("#F768A1", "#DD3497", "#AE017E",
             "#7A0177", "#49006A"),
  vfont = c("sans serif", "plain"),
  ...
) {
  wordcloud(
    words = words,
    freq = freq,
    min.freq = min.freq,
    max.words = max.words,
    random.order = random.order,
    colors = colors,
    vfont = vfont, 
    ...
  )
}