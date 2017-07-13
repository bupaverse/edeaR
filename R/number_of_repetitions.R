
#' @title Metric:  Number of repetitions
#'
#' @description  Provides summuary statistics on the number of repetitions, at the level of activity types, traces, cases and the eventlog.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type The type of repetitions, either repeat or redo
#'
#' @param level_of_analysis At which level the analysis of repetitions should be performed: log, case, activity, resource, resource-activity.
#'
#'
#' @export number_of_repetitions


number_of_repetitions <- function(eventlog,
								  type = c("repeat","redo"),
						level_of_analysis = c("log","case","activity","resource","resource-activity")){

	stop_eventlog(eventlog)

	type <- match.arg(type)
	level_of_analysis <- match.arg(level_of_analysis)

	if(type == "repeat") {
		output <- switch(level_of_analysis,
			   log = repeat_repetitions_log(eventlog),
			   case = repeat_repetitions_case(eventlog),
			   activity = repeat_repetitions_activity(eventlog),
			   resource = repeat_repetitions_resource(eventlog),
			   "resource-activity" = repeat_repetitions_resource_activity(eventlog)
		)
	}
	else if (type == "redo") {
		output <- switch(level_of_analysis,
			   log = redo_repetitions_log(eventlog),
			   case = redo_repetitions_case(eventlog),
			   activity = redo_repetitions_activity(eventlog),
			   resource = redo_repetitions_resource(eventlog),
			   "resource-activity" = redo_repetitions_resource_activity(eventlog)
		)

	}

	class(output) <- c("number_of_repetitions", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "type") <- type

	return(output)

}
