% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/homogenization.R
\name{homogenize_panel}
\alias{homogenize_panel}
\alias{bind_waves}
\title{Homogenize all waves to consistent structure}
\usage{
homogenize_panel(panel, mapping = NULL, ...)

bind_waves(panel, allow_issues = FALSE, ...)
}
\arguments{
\item{panel}{An unhomogenized panel}

\item{mapping}{A panel mapping. If NULL, a panel mapping must be attached
to the \code{panel} object using \code{add_mapping()}}

\item{...}{Parameters to be used for context, usually for defining a panel
schema}

\item{allow_issues}{If \code{TRUE}, will allow waves to be bound together even if
there are identified issues. Use caution with this!}
}
\value{
An \code{unhomogenized_panel} that is ready to be homogenized using
\code{bind_waves()}
}
\description{
Once all waves are collected into a single \code{unhomogenized_panel} object, this
will homogenize variable names and, where applicable, categorical codings
according to a panel mapping.
}
\section{Functions}{
\itemize{
\item \code{bind_waves()}: Bind waves into a homogenized panel after
successful homogenization

}}
\section{Homogenization Steps}{
The first part of the homogenization process is to harmonize wave variable names
to the homogenized name. If either there are missing wave variable names and a provided
homogenized name \emph{or} provided variable names and a missing homogenized name, an error
will be thrown. The original version of panelcleaner included a notion of "issues"
that would allow harmonization with errors, but after continued practice of
using panelcleaner, this behavior is deprecated in favor of halting the harmonization
process altogether.

The next step is to harmonize the codings for categorical data. As panelcleaner was
intended to be used in a data processing pipeline before analysis was conducted in
Stata, the desired behavior of panelcleaner is to separate values from labels, unlike
R's \code{factor} class. Codings are written using \code{\link[rcoder:coding]{rcoder::coding()}}. The harmonization process
is similar to names: errors will be thrown if wave codings and homogenized codings
aren't both present or missing, and the codings in all waves will be recoded to the
homogenized coding.
\subsection{Descriptions}{

The last step is to harmonize variable descriptions. This part is optional. It will
only happen if the \code{homogenized_description} (or custom name specified with a custom
panelcleaner schema) is present. The same types will occur for descriptions. The only
thing different about harmonizing descriptions is that it doesn't affect the data:
it operates by assigning the \code{bpr.description} attribute for variables. This feature
is really only useful if you intend you data to be used in a
\href{https://nyuglobalties.github.io/blueprintr}{blueprintr} project.
}
}

\section{Extra Parameters}{
In some cases the default behavior of panelcleaner is too restrictive, especially during
the beginning of data collection. Often, APIs or general data exports don't include
variables that don't have any submissions yet, but you still want to keep those variables
in your input data. These parameters lift some restrictions on panelcleaner's behavior:
\itemize{
\item \code{drop_na_homogenized}: If \code{TRUE}, any NA entries in the homogenized_name column will be
ignored, as if the row in the panel mapping doesn't exist.
\item \code{ignored_missing_codings}: If \code{TRUE}, waves with NA codings but with non-NA homogenized
codings will not have their values homogenized.
\item \code{ignored_missing_homogenized_codings}: If \code{TRUE}, any variables that have defined wave
codings but no homogenized coding will not have their codings homogenized.
\item \code{error_missing_raw_variables}: If \code{FALSE}, raw variables that should be present in the
data, given the panel mapping, but aren't will not throw an error. Instead, they'll be
added to the list of \link[=issues]{issues}.
\item \code{replace_missing_with_na}: If \code{TRUE}, raw_variables that should be present in the data,
given the panel mapping, but are not will be created and filled with NA values. A message
will be displayed of all the variables where this action was applied. This value supersedes
\code{error_missing_raw_variables}.
}
}

