#' @title Metric: Resource Specialisation
#'
#' @description Analyses whether resources specialise in specific activities
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case, resource.
#'
#' @export resource_specialisation


resource_specialisation <- function(eventlog, level_of_analysis = c("log","case","resource","activity")) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)

	if(level_of_analysis == "log")
		output <- resource_specialisation_log(eventlog = eventlog)
	else if(level_of_analysis == "case")
		output <- resource_specialisation_case(eventlog = eventlog)
	else if(level_of_analysis == "activity")
		output <- resource_specialisation_activity(eventlog = eventlog)
	else
		output <- resource_specialisation_resource(eventlog = eventlog)



	class(output) <- c("resource_specialisation", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}
