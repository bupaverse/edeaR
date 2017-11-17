#' @title Metric: Number of selfloops in trace
#'
#' @description Returns the number of selfloops in each trace. Can be performed at the level of traces, activities, or the level of the event log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type The type of selfloops, either repeat or redo.
#' @param level At which level the analysis of selfloops should be performed: log, case, activity, resource or resource-activity.
#' @param append Logical indicating whether the result should be appended to the orignal event log
#' @param ... Deprecated arguments
#' @export number_of_selfloops

number_of_selfloops <- function(eventlog, type, level, append, ...) {
	UseMethod("number_of_selfloops")
}

#' @describeIn number_of_selfloops Compute number of selfloops for eventlog
#' @export

number_of_selfloops.eventlog <- function(eventlog,
								type = c("repeat","redo"),
								level = c("log","case","activity","resource","resource-activity"),
								append = F,
								...) {

	stop_eventlog(eventlog)
	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	mapping <- mapping(eventlog)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_log,
					  case = repeat_selfloops_case,
					  activity = repeat_selfloops_activity,
					  resource = repeat_selfloops_resource,
					  "resource-activity" = repeat_selfloops_resource_activity
		)

	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_log,
					  case = redo_selfloops_case,
					  activity = redo_selfloops_activity,
					  resource = redo_selfloops_resource,
					  "resource-activity" = redo_selfloops_resource_activity
		)
	}

	output <- FUN(eventlog = eventlog)

	output <- return_metric(eventlog, output, level, append, "number_of_selfloops")

	attr(output, "type") <- type

	return(output)

}

#' @describeIn number_of_selfloops Compute number of selfloops for grouped eventlog
#' @export

number_of_selfloops.grouped_eventlog <- function(eventlog,
								type = c("repeat","redo"),
								level = c("log","case","activity","resource","resource-activity"),
								append = F,
								...) {

	stop_eventlog(eventlog)
	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)


	mapping <- mapping(eventlog)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_log,
					  case = repeat_selfloops_case,
					  activity = repeat_selfloops_activity,
					  resource = repeat_selfloops_resource,
					  "resource-activity" = repeat_selfloops_resource_activity
		)

	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_log,
					  case = redo_selfloops_case,
					  activity = redo_selfloops_activity,
					  resource = redo_selfloops_resource,
					  "resource-activity" = redo_selfloops_resource_activity
		)
	}

		if(level != "log") {
			output <- grouped_metric(eventlog, FUN)
		}
		else {
			output <- grouped_metric_raw_log(eventlog, FUN)
		}

	output <- return_metric(eventlog, output, level, append, "number_of_selfloops", ifelse(level == "resource-activity", 3,2))

	attr(output, "type") <- type

	return(output)

}

