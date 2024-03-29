% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel.R
\name{add_mapping}
\alias{add_mapping}
\alias{add_mapping.default}
\alias{add_mapping.unhomogenized_panel}
\title{Add a variable mapping to a panel}
\usage{
add_mapping(panel, mapping, ...)

\method{add_mapping}{default}(panel, mapping, ...)

\method{add_mapping}{unhomogenized_panel}(panel, mapping, ...)
}
\arguments{
\item{panel}{A panel object}

\item{mapping}{A \code{data.frame} to be used to map out variables over each wave}

\item{...}{Parameters passed onto other methods}
}
\value{
The input panel with a new mapping
}
\description{
Add a variable mapping to a panel
}
\section{Methods (by class)}{
\itemize{
\item \code{add_mapping(default)}: The default method

\item \code{add_mapping(unhomogenized_panel)}: Method for unhomogenized panels

}}
