#' @title Metric: Number of selfloops in trace
#'
#' @description Returns the number of selfloops in each trace. Can be performed at the level of traces, activities, or the level of the event log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type The type of selfloops, either repeat or redo.
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: log, case, activity, resource or resource-activity.
#' @export number_of_selfloops


number_of_selfloops <- function(eventlog,
								type = c("repeat","redo"),
								level_of_analysis = c("log","case","activity","resource","resource-activity")) {

	stop_eventlog(eventlog)
	type <- match.arg(type)
	level_of_analysis <- match.arg(level_of_analysis)

	if(type == "repeat") {
		output <- switch(level_of_analysis,
			   log = repeat_selfloops_log(eventlog),
			   case = repeat_selfloops_case(eventlog),
			   activity = repeat_selfloops_activity(eventlog),
			   resource = repeat_selfloops_resource(eventlog),
			   "resource-activity" = repeat_selfloops_resource_activity(eventlog)
		)
	}
	else if (type == "redo") {
		output <- switch(level_of_analysis,
			   log = redo_selfloops_log(eventlog),
			   case = redo_selfloops_case(eventlog),
			   activity = redo_selfloops_activity(eventlog),
			   resource = redo_selfloops_resource(eventlog),
			  "resource-activity" = redo_selfloops_resource_activity(eventlog)
		)

		class(output) <- c("number_of_selfloops", class(output))
		attr(output, "level") <- level_of_analysis
		attr(output, "mapping") <- mapping(eventlog)
		attr(output, "type") <- type


		return(output)
	}
}
