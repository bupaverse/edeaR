#' @title Metric: Activity Presence
#'
#' @description Calculates for each activity type in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#'
#' @examples
#'
#' data(example_log)
#' activity_presence(example_log)
#'
#'
#' @export activity_presence


activity_presence <- function(eventlog) {
	stop_eventlog(eventlog)

	eventlog %>% rename_("case_classifier" = case_id(eventlog)) %>%
		group_by_(activity_id(eventlog)) %>%
		summarize("absolute" = n_distinct(case_classifier)) %>%
		mutate(relative = absolute/n_cases(eventlog)) %>%
		arrange(-absolute) %>%
	return()
}
