#' @title Metric: Activity Presence in traces
#'
#' @description Calculates for each activity type in what percentage of traces it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#' @seealso \code{\link{activity_type_frequency}}
#'
#' @examples
#'
#' data(example_log)
#' activity_presence_in_traces(example_log)
#'
#'
#' @export activity_presence_in_traces


activity_presence_in_traces <- function(eventlog) {
	library(dplyr)

	stop_eventlog(eventlog)

	e <- eventlog


	ca <- cases_light(eventlog)
	ntraces <- length(unique(ca$trace))

	e <- merge(e, ca, by = colnames(ca)[1])

	event_classifier <- activity_id(eventlog)
	r <- group_by_(e, "trace",event_classifier) %>%
		summarize()

	r <- group_by_(r, event_classifier) %>%
		summarize(absolute = n_distinct(trace)) %>%
		ungroup() %>%
		arrange(desc(absolute))

	r$relative <- r$absolute/ntraces
	colnames(r)[colnames(r)=="event_classifier"] <- activity_id(eventlog)

	return(r)
}
