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
#' @export resource_frequency


resource_frequency <- function(eventlog, level_of_analysis) {
	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("log","case", "activity","resource","resource-activity")))
		stop("Level of analysis should be one of the following: log, case, activity, resource, resource-activity.")

	if(level_of_analysis == "log")
		return(resource_frequency_log(eventlog = eventlog))
	else if(level_of_analysis == "case")
		return(resource_frequency_case(eventlog = eventlog))
	else if(level_of_analysis == "activity")
		return(resource_frequency_activity(eventlog = eventlog))
	else if(level_of_analysis == "resource")
		return(resource_frequency_resource(eventlog = eventlog))
	else
		return(resource_frequency_resource_activity(eventlog = eventlog))
}
