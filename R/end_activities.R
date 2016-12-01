#'@title Metric: End activities
#'
#'@description At log level, computes how many activity types occur as the last event in a case, both absolute and relative.
#'At activity level, shows the activities which occur as last, and how often.
#'The last event in a case is the one which completed the last.
#'
#' @param level_of_analysis At which level the analysis of end activities should be performed: log, case, activity, resource or resource-activity.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'

#'@export end_activities


end_activities <- function(eventlog,
						   level_of_analysis) {
	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("log", "case", "activity", "resource", "resource-activity")))
		stop("Level of analysis should be one of the following: log, case, activity, resource, resource-activity.")

	if (level_of_analysis == "log")
		return(end_activities_log(eventlog = eventlog))
	else if (level_of_analysis == "case")
		return(end_activities_case(eventlog = eventlog))
	else if (level_of_analysis == "activity")
		return(end_activities_activity(eventlog = eventlog))
	else if(level_of_analysis == "resource")
		return(end_activities_resource(eventlog = eventlog))
	else
		return(end_activities_resource_activity(eventlog = eventlog))
}
