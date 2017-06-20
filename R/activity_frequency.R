#' @title Metric: Activity Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of traces, cases, resources or activity types.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: log, trace, case, activity.
#'
#' @export activity_frequency

activity_frequency <- function(eventlog,
									level_of_analysis = c("log","trace","activity","case")) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)

	if (level_of_analysis == "trace")
		output <- activity_frequency_trace(eventlog)
	else if (level_of_analysis == "case")
		output <- activity_frequency_case(eventlog)
	else if(level_of_analysis == "activity")
		output <- activity_frequency_activity(eventlog)
	else if (level_of_analysis == "log")
		output <- activity_frequency_log(eventlog)

	class(output) <- c("activity_frequency", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}
