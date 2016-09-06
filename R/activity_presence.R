#' @title Metric: Activity Presence
#'
#' @description Calculates for each activity type in what percentage of cases it is present.
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
#' activity_presence(example_log)
#'
#'
#' @export activity_presence


activity_presence <- function(eventlog) {
	stop_eventlog(eventlog)

	r <- eventlog %>%
		group_by_(activity_id(eventlog), case_id(eventlog)) %>%
		summarize() %>%
		summarize("absolute" = n()) %>%
		mutate(relative = absolute/n_cases(eventlog)) %>%
		arrange(-absolute)
	return(r)
}
