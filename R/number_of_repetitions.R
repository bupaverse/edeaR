
#' @title Metric:  Number of repetitions
#'
#' @description  Provides summuary statistics on the number of repetitions, at the level of activity types, traces, cases and the eventlog.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type The type of repetitions, either repeat or redo
#'
#' @param level At which level the analysis of repetitions should be performed: log, case, activity, resource, resource-activity.
#' @param append Logical indicating whether result should be appended to original data
#' @param ... Deprecated arguments
#'
#' @export number_of_repetitions


number_of_repetitions <- function(eventlog, type, level, append, ...) {
	UseMethod("number_of_repetitions")
}

#' @describeIn number_of_repetitions ""
#' @export

number_of_repetitions.eventlog <- function(eventlog,
								  type = c("repeat","redo"),
								  level = c("log","case","activity","resource","resource-activity"),
								  append = F,
								  ...){

	mapping <- mapping(eventlog)
	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_log,
					  case = repeat_repetitions_case,
					  activity = repeat_repetitions_activity,
					  resource = repeat_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_repetitions_log,
					  case = redo_repetitions_case,
					  activity = redo_repetitions_activity,
					  resource = redo_repetitions_resource,
					  "resource-activity" = redo_repetitions_resource_activity
		)
	}

	output <- FUN(eventlog = eventlog)


	output <- return_metric(eventlog, output, level, append, "number_of_repetitions", ifelse(level == "resource-activity", 3,2))
	attr(output, "type") <- type

	return(output)

}


#' @describeIn number_of_repetitions ""
#' @export

number_of_repetitions.grouped_eventlog <- function(eventlog,
												   type = c("repeat","redo"),
												   level = c("log","case","activity","resource","resource-activity"),
												   append = F,
												   ...){

	mapping <- mapping(eventlog)
	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_log,
					  case = repeat_repetitions_case,
					  activity = repeat_repetitions_activity,
					  resource = repeat_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_repetitions_log,
					  case = redo_repetitions_case,
					  activity = redo_repetitions_activity,
					  resource = redo_repetitions_resource,
					  "resource-activity" = redo_repetitions_resource_activity
		)

	}

	if(level != "log") {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}



	output <- return_metric(eventlog, output, level, append, "number_of_repetitions", ifelse(level == "resource-activity", 3,2))
	attr(output, "type") <- type

	return(output)

}
