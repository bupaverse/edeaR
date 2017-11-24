#' @title Metric: Processing time
#'
#' @description Provides summary statistics about the processing time of events on the level of activities, traces, cases or log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#' @param append Logical indicating whether the result should be appended to the original data.
#' @param level At which level the analysis of processing times should be performed: log, trace, case, resource or activity.
#' @param ... Deprecated arguments
#' @export processing_time

processing_time <- function(eventlog, level, append, units, ...) {
	UseMethod("processing_time")
}

#' @describeIn processing_time Compute processing time for event log
#' @export

processing_time.eventlog <- function(eventlog,
							level = c("log","trace","case","activity","resource","resource-activity"),
							append = F,
							units = c("hours","days","weeks","mins"),
							...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	FUN <- switch(level,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	output <- FUN(eventlog = eventlog, units = units)

	return_metric(eventlog, output, level, append, "processing_time", ifelse(level == "case", 1, 9))

}


#' @describeIn processing_time Compute processing time on grouped eventlog
#' @export

processing_time.grouped_eventlog <- function(eventlog,
							level = c("log","trace","case","activity","resource","resource-activity"),
							append = F,
							units = c("hours","days","weeks","mins"),
							...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	FUN <- switch(level,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	if(!(level %in% c("log","activity","resource-activity","resource"))) {
		output <- grouped_metric(eventlog, FUN, units)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN, units)
	}
	return_metric(eventlog, output, level, append, "processing_time", ifelse(level == "case", 1, 9))

}
