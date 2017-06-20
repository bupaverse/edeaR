#' @title Metric: Processing time
#'
#' @description Provides summary statistics about the processing time of events on the level of activities, traces, cases or log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#' @param level_of_analysis At which level the analysis of processing times should be performed: log, trace, case, resource or activity.
#'#'
#' @export processing_time

processing_time <- function(eventlog,
							level_of_analysis = c("log","trace","case","activity","resource","resource-activity"),
							units = c("hours","days","weeks","mins")){

	level_of_analysis <- match.arg(level_of_analysis)
	units <- match.arg(units)

	stop_eventlog(eventlog)

	if(level_of_analysis == "trace"){
		output <- processing_time_trace(eventlog = eventlog, units = units)
	}
	else if(level_of_analysis == "log") {
		output <- processing_time_log(eventlog = eventlog, units = units)
	}
	else if(level_of_analysis == "case") {
		output <- processing_time_case(eventlog = eventlog, units = units)
	}
	else if(level_of_analysis == "activity") {
		output <- processing_time_activity(eventlog = eventlog, units = units)
	}
	else if(level_of_analysis == "resource"){
		output <- processing_time_resource(eventlog = eventlog, units = units)
	}
	else {
		output <- processing_time_resource_activity(eventlog = eventlog, units = units)
	}


	class(output) <- c("processing_time", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)

}
