#' @title Filter: Activity Presence
#'
#' @description Filters cases based on the presence (or absence) of activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param activities A vector of activities to withhold.
#'
#' @param method Filter method. If "all", each of the activities should be present. If "one_of", at least one of them should be present. If "none",
#'
#' @export filter_activity_presence
#'
filter_activity_presence <- function(eventlog,
							activities = NULL,
							method = "all"){
	stop_eventlog(eventlog)

	e <- eventlog
	colnames(e)[colnames(e) == activity_id(e)] <- "event_classifier"
	colnames(e)[colnames(e) == case_id(e)] <- "case_classifier"

	e %>%
		filter(event_classifier %in% activities) %>%
		select(event_classifier, case_classifier) %>%
		unique() %>%
		group_by(case_classifier) %>%
		summarize(n = n()) -> selection

	selection %>%
		filter(n == length(activities)) -> selection_all

	if(method == "all")
		output <- filter_case(eventlog, selection_all$case_classifier)

	else if(method == "one_of")
		output <- filter_case(eventlog, selection$case_classifier)
	else if (method == "none")
		output <- filter_case(eventlog, selection$case_classifier, reverse = T)
	else
		stop("Method should be all, one_of or none.")

	return(output)
}
