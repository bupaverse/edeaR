#' @title Metric: Processing time
#'
#' @description Provides summary statistics about the processing time of events on the level of activities or traces
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#' @param level_of_analysis At which level the analysis of processing times should be performed: trace or activity.
#'
#' @export processing_time

processing_time <- function(eventlog,
							units = "days",
							level_of_analysis){

	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("trace","activity")))
		stop("Level of analysis should be one of the following: trace, activity.")

	if(level_of_analysis == "trace"){
		return(processing_time_trace(eventlog = eventlog, units = units))
	}
	else {
		return(processing_time_activity(eventlog = eventlog, units = units))
	}
}
