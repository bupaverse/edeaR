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
									level_of_analysis) {
	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("trace", "activity","case", "log")))
		stop("Level of analysis should be one of the following: log,  trace, case, activity.")

	if (level_of_analysis == "trace")
		return(activity_type_frequency_trace(eventlog))
	else if (level_of_analysis == "case")
		return(activity_type_frequency_case(eventlog))
	else if(level_of_analysis == "activity")
		return(activity_type_frequency_activity(eventlog))
	else if (level_of_analysis == "log")
		return(activity_type_frequency_log(eventlog))
}
