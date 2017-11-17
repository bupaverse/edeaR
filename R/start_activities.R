#'@title Metric: Start activities
#'
#'@description At log level, computes how many activity types occur as the first event in a case, both absolute and relative.
#'At activity level, shows the activities which occur as first, and how often.
#'The first event in a case is the one which started the first.
#'
#' @inheritParams end_activities
#'
#'@export start_activities

start_activities <- function(eventlog, level, append = F, ...) {
	UseMethod("start_activities")
}

#' @describeIn start_activities Start activities for eventlog
#' @export

start_activities.eventlog <- function(eventlog,
							 level = c("log","case","activity","resource","resource-activity"),
							 append = F,
							 ...) {

	level <- match.arg(level)
	mapping <- mapping(eventlog)

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "start_activities", ifelse(level == "case",1,3))

}

#' @describeIn start_activities Start activities for grouped eventlog
#' @export

start_activities.grouped_eventlog <- function(eventlog,
							 level = c("log","case","activity","resource","resource-activity"),
							 append = F,
							 ...) {

	level <- match.arg(level)
	mapping <- mapping(eventlog)

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	grouped_metric(eventlog, FUN) -> output

	return_metric(eventlog, output, level, append, "start_activities", ifelse(level == "case",1,3))

}
