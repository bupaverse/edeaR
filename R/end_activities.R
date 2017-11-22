#'@title Metric: End activities
#'
#'@description At log level, computes how many activity types occur as the last event in a case, both absolute and relative.
#'At activity level, shows the activities which occur as last, and how often.
#'The last event in a case is the one which completed the last.
#'
#' @param level At which level the analysis of end activities should be performed: log, case, activity, resource or resource-activity.
#' @param append Logical indicating whether the results should be appended to the original event log.
#' @param ... Deprecated arguments
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'

#' @export end_activities

end_activities <- function(eventlog, level, append,  ...) {
	UseMethod("end_activities")
}

#' @describeIn end_activities Compute end activities for eventlog
#' @export

end_activities.eventlog <- function(eventlog,
									level = c("log","case","activity","resource","resource-activity"),
									append = F,
									...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "end_activities", n_result_col = ifelse(level == "case",1,3))

}

#' @describeIn end_activities Compute end activities for grouped eventlog
#' @export

end_activities.grouped_eventlog <- function(eventlog,
											level = c("log","case","activity","resource","resource-activity"),
											append = F,
											...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- grouped_metric(eventlog, FUN)

	return_metric(eventlog, output, level, append, "end_activities", n_result_col = ifelse(level == "case",1,3))

}
