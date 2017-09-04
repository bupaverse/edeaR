#'@title Metric: Start activities
#'
#'@description At log level, computes how many activity types occur as the first event in a case, both absolute and relative.
#'At activity level, shows the activities which occur as first, and how often.
#'The first event in a case is the one which started the first.
#'#'
#' @param level_of_analysis At which level the analysis of start activities should be performed: log, case, activity, resource and resource-activity.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'@export start_activities

start_activities <- function(eventlog,
							 level_of_analysis = c("log","case","activity","resource","resource-activity")) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)
	mapping <- mapping(eventlog)

	FUN <- switch(level_of_analysis,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)


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

	class(output) <- c("start_activities", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)

}
