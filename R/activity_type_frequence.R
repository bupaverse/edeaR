#' @title Metric: Activity Type Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of traces or activity types
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: trace or activity.
#'
#' @export activity_type_frequency

activity_type_frequency <- function(eventlog,
									level_of_analysis) {
	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("trace", "activity")))
		stop("Level of analysis should be one of the following: trace, activity.")

	if (level_of_analysis == "trace")
		return(activity_type_frequency_trace(eventlog))
	else
		return(activity_type_frequency_activity(eventlog))
}
