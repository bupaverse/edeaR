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
	mapping <- mapping(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)

	FUN <- switch(level_of_analysis,
				  case = resource_involvement_case,
				  resource = resource_involvement_resource,
				  "resource-activity" = resource_involvement_resource_activity)



	if("grouped_eventlog" %in% class(eventlog)) {
		eventlog %>%
			nest %>%
			mutate(data = map(data, re_map, mapping)) %>%
			mutate(data = map(data, FUN)) %>%
			unnest -> output

		attr(output, "groups") <- groups(eventlog)
	}
	else{
		output <- FUN(eventlog = eventlog)
	}

	class(output) <- c("resource_involvement", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)
}
