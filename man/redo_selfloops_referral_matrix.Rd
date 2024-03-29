% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redo_selfloops_referral_matrix.R
\name{redo_selfloops_referral_matrix}
\alias{redo_selfloops_referral_matrix}
\alias{redo_selfloops_referral_matrix.eventlog}
\alias{redo_selfloops_referral_matrix.activitylog}
\title{Referral matrix selfloops}
\usage{
redo_selfloops_referral_matrix(log, eventlog = deprecated())

\method{redo_selfloops_referral_matrix}{eventlog}(log, eventlog = deprecated())

\method{redo_selfloops_referral_matrix}{activitylog}(log, eventlog = deprecated())
}
\arguments{
\item{log}{\code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).}

\item{eventlog}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}; please use \code{log} instead.}
}
\description{
Provides a list of initatiors and completers of  redo selfloops
}
\section{Methods (by class)}{
\itemize{
\item \code{redo_selfloops_referral_matrix(eventlog)}: Compute matrix for eventlog

\item \code{redo_selfloops_referral_matrix(activitylog)}: Compute matrix for activitylog

}}
\references{
Swennen, M. (2018). Using Event Log Knowledge to Support Operational Exellence Techniques (Doctoral dissertation). Hasselt University.
}
\seealso{
\code{\link{number_of_selfloops}}
}
\concept{metrics_repetition}
