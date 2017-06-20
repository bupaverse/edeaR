#' @title Metric: Resource frequency
#'
#' @description Analyses the frequency of resources at different levels of analysis
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case, activity, resource, resource-activity.
#'
#' @param include_zeros At the resource-activity level, include pairs which do not occur.
#'
#' @export resource_frequency


resource_frequency <- function(eventlog, level_of_analysis = c("log","case","activity","resource","resource-activity"),
							   include_zeros = F) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)

	if(level_of_analysis == "log")
		output <- resource_frequency_log(eventlog = eventlog)
	else if(level_of_analysis == "case")
		output <- resource_frequency_case(eventlog = eventlog)
	else if(level_of_analysis == "activity")
		output <- resource_frequency_activity(eventlog = eventlog)
	else if(level_of_analysis == "resource")
		output <- resource_frequency_resource(eventlog = eventlog)
	else
		output <- suppressWarnings(resource_frequency_resource_activity(eventlog = eventlog, include_zeros = include_zeros))


	class(output) <- c("resource_frequency", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)




}
