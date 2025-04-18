% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcite_citation_page.R
\name{gcite_citation_page}
\alias{gcite_citation_page}
\alias{gcite_citation_page.xml_nodeset}
\alias{gcite_citation_page.xml_document}
\alias{gcite_citation_page.character}
\alias{gcite_citation_page.list}
\alias{gcite_citation_page.default}
\title{Parse Google Citation Index}
\usage{
gcite_citation_page(doc, title = NULL, force = FALSE, ...)

\method{gcite_citation_page}{xml_nodeset}(doc, title = NULL, force = FALSE, ...)

\method{gcite_citation_page}{xml_document}(doc, title = NULL, force = FALSE, ...)

\method{gcite_citation_page}{character}(doc, title = NULL, force = FALSE, ...)

\method{gcite_citation_page}{list}(doc, title = NULL, force = FALSE, ...)

\method{gcite_citation_page}{default}(doc, title = NULL, force = FALSE, ...)
}
\arguments{
\item{doc}{A xml_document or the url for the main page}

\item{title}{title of the article}

\item{force}{If passing a URL and there is a failure, should the 
program return \code{NULL}?}

\item{...}{arguments passed to \code{\link[httr]{GET}}}
}
\value{
A matrix of indices
}
\description{
Parses a google citation indices (h-index, etc.) from main page
}
\examples{
library(httr)
library(rvest)
url = paste0("https://scholar.google.com/citations?view_op=view_citation&", 
"hl=en&oe=ASCII&user=T9eqZgMAAAAJ&pagesize=100&", 
"citation_for_view=T9eqZgMAAAAJ:W7OEmFMy1HYC")
url = gcite_url(url = url, pagesize = 10, cstart = 0) 
if (!is_travis() & !is_cran()) {
ind = gcite_citation_page(url)
doc = content(httr::GET(url))
ind = gcite_citation_page(doc)
ind_nodes = html_nodes(doc, "#gsc_oci_table div")
ind_nodes = html_nodes(ind_nodes, xpath = '//div[@class = "gs_scl"]')  
ind = gcite_citation_page(ind_nodes)
}
}
