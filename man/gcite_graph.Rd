% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcite_graph.R
\name{gcite_graph}
\alias{gcite_graph}
\alias{gcite_graph.xml_node}
\alias{gcite_graph.xml_document}
\alias{gcite_graph.character}
\alias{gcite_graph.default}
\title{Parse Google Citation Graph}
\usage{
gcite_graph(citations, ...)

\method{gcite_graph}{xml_node}(citations, ...)

\method{gcite_graph}{xml_document}(citations, ...)

\method{gcite_graph}{character}(citations, ...)

\method{gcite_graph}{default}(citations, ...)
}
\arguments{
\item{citations}{A list of nodes or xml_node}

\item{...}{arguments passed to \code{\link[httr]{GET}}}
}
\value{
A matrix of citations and years
}
\description{
Parses a google citation bar graph from html
}
