#' @title Metric: Throughput time of cases
#'
#' @description  Provides summary statistics concerning the throughput times of cases.
#' The throughput time of cases is defined as the time between the start of the first event and the completion of the last event.
#' Can be performed at the level of the log as well as the level of traces and cases.
#'
#' @inheritParams idle_time
#'
#'
#'
#' @export throughput_time
#'


throughput_time <- function(eventlog, level, append, units, ...) {
	UseMethod("throughput_time")
}

#' @describeIn throughput_time Throughput time for eventlog
#' @export


throughput_time.eventlog <- function(eventlog,
									 level = c("log","trace","case"),
									 append = FALSE,
									 units = c("days", "hours","mins","weeks"),
									 ...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)


	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)


	output <- FUN(eventlog = eventlog, units = units)

	output <- return_metric(eventlog, output, level, append, "throughput_time", 1)
	attr(output, "units") <- units

	return(output)
}


#' @describeIn throughput_time Throughput time for grouped eventlog
#' @export

throughput_time.grouped_eventlog <- function(eventlog,
											 level = c("log","trace","case"),
											 append = FALSE,
											 units = c("days", "hours","mins","weeks"),
											 ...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)


	if(level != "log") {
		grouped_metric(eventlog, FUN, units) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN, units) -> output
	}

	output <- return_metric(eventlog, output, level, append, "throughput_time", 1)
	attr(output, "units") <- units

	return(output)
}
