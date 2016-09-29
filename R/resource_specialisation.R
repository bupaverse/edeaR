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


resource_specialisation <- function(eventlog, level_of_analysis) {
	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("log","case", "resource", "activity")))
		stop("Level of analysis should be one of the following: log, case, activity, resource")

	if(level_of_analysis == "log")
		return(resource_specialisation_log(eventlog = eventlog))
	else if(level_of_analysis == "case")
		return(resource_specialisation_case(eventlog = eventlog))
	else if(level_of_analysis == "activity")
		return(resource_specialisation_activity(eventlog = eventlog))
	else
		return(resource_specialisation_resource(eventlog = eventlog))
}
