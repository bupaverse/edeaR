#' @title Metric: Resource Involvement
#'
#' @description Calculates for each resource/resource-activity in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis The level of analysis: resource or resource-activity.
#'
#' @export resource_involvement

resource_involvement <- function(eventlog, level_of_analysis = c("case","resource","resource-activity")) {

	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)



	if(level_of_analysis == "resource")
		output <- resource_involvement_resource(eventlog = eventlog)
	else if(level_of_analysis == "case")
		output <- resource_involvement_case(eventlog = eventlog)
	else
		output <- resource_involvement_resource_activity(eventlog = eventlog)

	class(output) <- c("resource_involvement", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)
}
