#' @title Metric: Size of repetitions
#'
#' @description Provides summary statistics on the sizes of repetitions at the level of activity types, cases, traces or log. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#'
#' @inheritParams number_of_selfloops
#'
#' @export size_of_repetitions
#'
size_of_repetitions <- function(eventlog, type, level, append, ...) {
	UseMethod("size_of_repetitions")
}
#' @describeIn size_of_repetitions Size of repetitions for eventlog
#' @export

size_of_repetitions.eventlog <- function(eventlog,
								type = c("repeat","redo"),
								level = c("log","case","activity","resource","resource-activity"),
								append = FALSE,
								...){

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_size_log,
					  case = repeat_repetitions_size_case,
					  activity = repeat_repetitions_size_activity,
					  resource = repeat_repetitions_size_resource,
					  "resource-activity" = repeat_repetitions_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- 	switch(level,
					   log = redo_repetitions_size_log,
					   case = redo_repetitions_size_case,
					   activity = redo_repetitions_size_activity,
					   resource = redo_repetitions_size_resource,
					   "resource-activity" = redo_repetitions_size_resource_activity
		)

	}

	output <- FUN(eventlog = eventlog)


	output <- return_metric(eventlog, output, level, append, "size_of_repetitions", 8)
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_repetitions Size of repetitions for grouped event log
#' @export

size_of_repetitions.grouped_eventlog <- function(eventlog,
												 type = c("repeat","redo"),
												 level = c("log","case","activity","resource","resource-activity"),
												 append = FALSE,
												 ...){

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_size_log,
					  case = repeat_repetitions_size_case,
					  activity = repeat_repetitions_size_activity,
					  resource = repeat_repetitions_size_resource,
					  "resource-activity" = repeat_repetitions_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- 	switch(level,
					   log = redo_repetitions_size_log,
					   case = redo_repetitions_size_case,
					   activity = redo_repetitions_size_activity,
					   resource = redo_repetitions_size_resource,
					   "resource-activity" = redo_repetitions_size_resource_activity
		)

	}

	if(level != "log") {
		output <- grouped_metric(eventlog, FUN)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN)
	}


	output <- return_metric(eventlog, output, level, append, "size_of_repetitions", 8)
	attr(output, "type") <- type
	return(output)
}
