#' @title Metric: Resource Involvement
#'
#' @description Calculates for each resource in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#'
#' @export resource_involvement


resource_involvement <- function(eventlog) {
	stop_eventlog(eventlog)

	event_classifier <- activity_id(eventlog)
	case_classifier <- case_id(eventlog)
	resource_classifier <- resource_id(eventlog)
	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"

	r <- eventlog %>%
		group_by_(resource_classifier, "case_classifier") %>%
		summarize() %>%
		summarize("absolute" = n_distinct(case_classifier)) %>%
		mutate(relative = absolute/n_cases(eventlog)) %>%
		arrange(desc(absolute))
	return(r)
}
