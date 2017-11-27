#' @title Metric: Size of selfloops
#'
#' @description Provides summary statistics on the sizes of selfloops at the level of activity types, cases, resources, resource-activity combinations or log. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#' @inheritParams number_of_selfloops
#'
#' @export size_of_selfloops

size_of_selfloops <- function(eventlog, type, level, append, ...) {
	UseMethod("size_of_selfloops")
}

#' @describeIn size_of_selfloops Size of selfloops for eventlog
#' @export

size_of_selfloops.eventlog <- function(eventlog,
							  type = c("repeat","redo"),
							  level = c("log","case","activity","resource","resource-activity"),
							  append = F,
							  ...){

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	mapping <- mapping(eventlog)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_size_log,
					  case = repeat_selfloops_size_case,
					  activity = repeat_selfloops_size_activity,
					  resource = repeat_selfloops_size_resource,
					  "resource-activity" = repeat_selfloops_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_size_log,
					  case = redo_selfloops_size_case,
					  activity = redo_selfloops_size_activity,
					  resource = redo_selfloops_size_resource,
					  "resource-activity" = redo_selfloops_size_resource_activity
		)

	}

	output <- FUN(eventlog = eventlog)

	output <- return_metric(eventlog, output, level, append, "size_of_selfloops", 8)
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_selfloops Size of selfloops for grouped eventlog
#' @export

size_of_selfloops.grouped_eventlog <- function(eventlog,
							  type = c("repeat","redo"),
							  level = c("log","case","activity","resource","resource-acitivty"),
							  append = F,
							  ...){

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	mapping <- mapping(eventlog)

	if(type == "repeat") {
		FUN <- switch(level,
			   log = repeat_selfloops_size_log,
			   case = repeat_selfloops_size_case,
			   activity = repeat_selfloops_size_activity,
			   resource = repeat_selfloops_size_resource,
			   "resource-activity" = repeat_selfloops_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
			   log = redo_selfloops_size_log,
			   case = redo_selfloops_size_case,
			   activity = redo_selfloops_size_activity,
			   resource = redo_selfloops_size_resource,
			   "resource-activity" = redo_selfloops_size_resource_activity
		)

	}


		if(level != "log") {
			grouped_metric(eventlog, FUN) -> output
		}
		else {
			grouped_metric_raw_log(eventlog, FUN) -> output
		}




	output <- return_metric(eventlog, output, level, append, "size_of_selfloops", 8)
	attr(output, "type") <- type
	return(output)
}
