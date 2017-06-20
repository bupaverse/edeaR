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
						   level_of_analysis = c("log","case","activity","resource","resource-activity")) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)

	if (level_of_analysis == "log")
		output <- end_activities_log(eventlog = eventlog)
	else if (level_of_analysis == "case")
		output <- end_activities_case(eventlog = eventlog)
	else if (level_of_analysis == "activity")
		output <- end_activities_activity(eventlog = eventlog)
	else if(level_of_analysis == "resource")
		output <- end_activities_resource(eventlog = eventlog)
	else
		output <- end_activities_resource_activity(eventlog = eventlog)


	class(output) <- c("end_activities", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}
