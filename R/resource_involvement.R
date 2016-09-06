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

resource_involvement <- function(eventlog, level_of_analysis) {

	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("resource","resource-activity")))
		stop("Level of analysis should be one of the following: resource, resource-activity.")


	if(level_of_analysis == "resource")
		return(resource_involvement_resource(eventlog = eventlog))
	else
		return(resource_involvement_resource_activity(eventlog = eventlog))

}
