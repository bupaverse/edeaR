% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource_specialisation.R
\name{resource_specialisation}
\alias{resource_specialisation}
\alias{resource_specialization}
\alias{resource_specialisation.eventlog}
\alias{resource_specialisation.grouped_eventlog}
\title{Metric: Resource Specialisation}
\usage{
resource_specialisation(eventlog, level, append, ...)

resource_specialization(eventlog, level, append, ...)

\method{resource_specialisation}{eventlog}(
  eventlog,
  level = c("log", "case", "activity", "resource"),
  append = F,
  append_column = NULL,
  sort = TRUE,
  ...
)

\method{resource_specialisation}{grouped_eventlog}(
  eventlog,
  level = c("log", "case", "activity", "resource"),
  append = F,
  append_column = NULL,
  sort = TRUE,
  ...
)
}
\arguments{
\item{eventlog}{The dataset to be used. Should be a (grouped) eventlog object.
\code{eventlog}.}

\item{level}{Level of granularity for the analysis: log,  case, or resource.
For more information, see \code{vignette("metrics", "edeaR")}#'}

\item{append}{Logical, indicating whether to append results to original event log. Ignored when level is log or trace.}

\item{...}{Deprecated arguments}

\item{append_column}{Which of the output columns to append to log, if append = T. Default column depends on chosen level.}

\item{sort}{Sort output on count. Defaults to TRUE. Only for levels with frequency count output.}
}
\description{
Analyses whether resources specialise in specific activities
}
\details{
This can give a
company an overview of which resources are performing certain activities more
than others, and which resources are responsible for containing all knowledge
or capabilities on one topic.

\itemize{

\item On the level of the complete event log,
this metric provides summary statistics on the number of distinct activities executed per
resource.

\item On the level of the cases, this metric provides
the number of distinct activities that are executed within each case together
with the summary statistics of the distinct activities executed per resource
in each case.

\item On the level of the distinct activities,
this metric provides an overview of the absolute and relative number of different
resources executing this activity within the complete event log. This will give a
company insights in which activities resources are specialised in.

\item Finally, the resource specialisation can
also be calculated on the resource level, showing the absolute and relative number
of distinct activities that each resource executes.
}
}
\section{Methods (by class)}{
\itemize{
\item \code{eventlog}: Resource specialization for  eventlog

\item \code{grouped_eventlog}: Resource specialization for grouped eventlog
}}

\references{
Swennen, M. (2018). Using Event Log Knowledge to Support Operational Exellence Techniques (Doctoral dissertation). Hasselt University.
}
