#' @title Metric: Activity Presence
#'
#' @description Calculates for each activity type in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' @param append Logical indicating whether to append the result to the original event log
#' \code{eventlog}.
#'
#'
#' @examples
#' \dontrun{
#' data <- data.frame(case = rep("A",5),
#' activity_id = c("A","B","C","D","E"),
#' activity_instance_id = 1:5,
#' lifecycle_id = rep("complete",5),
#' timestamp = 1:5,
#' resource = rep("resource 1", 5))
#'
#' log <- bupaR::eventlog(data,case_id = "case",
#' activity_id = "activity_id",
#' activity_instance_id = "activity_instance_id",
#' lifecycle_id = "lifecycle_id",
#' timestamp = "timestamp",
#' resource_id = "resource")
#'
#'activity_presence(log)
#' }
#' @export activity_presence

activity_presence <- function(eventlog, append) {
	UseMethod("activity_presence")
}

#' @describeIn activity_presence Compute activity presence for event log
#' @export

activity_presence.eventlog <- function(eventlog, append = F) {

	FUN <- activity_presence_FUN
	mapping <- mapping(eventlog)
	output <- FUN(eventlog = eventlog)


	return_metric(eventlog, output, "activity", append, "activity_presence")
}

#' @describeIn activity_presence Compute activity presence for grouped eventlog
#' @export

activity_presence.grouped_eventlog <- function(eventlog, append = F) {

	FUN <- activity_presence_FUN

	mapping <- mapping(eventlog)
	output <- grouped_metric(eventlog, FUN)

	return_metric(eventlog, output, "activity", append, "activity_presence")
}

activity_presence_FUN <- function(eventlog) {
	eventlog %>%
		group_by(!!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n_distinct(!!as.symbol(case_id(eventlog)))) %>%
		mutate(relative = absolute/n_cases(eventlog)) %>%
		arrange(-absolute) -> output
	return(output)
}


