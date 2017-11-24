#' @title Metric: Idle Time
#'
#' @description Calculates the amount of time that no activity occurs for a case or for a resource. At log level it gives summary statistics of all cases in the log. At trace level it provides summary statistics of all cases related to this case.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level At which level the analysis of activity type frequency should be performed: log, trace, case, resource.
#' @param append Logical indicating whether to append results to original event log.
#' @param units Time units to be used
#' @param ...
#'
#' @export idle_time
#'
idle_time <- function(eventlog, level, append, units, ...) {
	UseMethod("idle_time")
}

#' @describeIn idle_time Compute the idle time for eventlog
#' @export

idle_time.eventlog <- function(eventlog,
							   level = c("log","case","trace","resource"),
							   append = FALSE,
							   units = c("hours","days", "weeks","mins"),
							   ...) {
	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)
	FUN <- switch(level,
				  log = idle_time_log,
				  case = idle_time_case,
				  trace = idle_time_trace,
				  resource = idle_time_resource)

	output <- FUN(eventlog = eventlog, units = units)
	return_metric(eventlog, output, level, append, "idle_time", 1)
}

#' @describeIn idle_time Compute idle time for grouped eventlog
#' @export

idle_time.grouped_eventlog <- function(eventlog,
									   level = c("log","case","trace","resource"),
									   append = FALSE,
									   units = c("hours","days", "weeks","mins"),
									   ...) {
	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	FUN <- switch(level,
				  log = idle_time_log,
				  case = idle_time_case,
				  trace = idle_time_trace,
				  resource = idle_time_resource)
	if(level != "log") {
		grouped_metric(eventlog, FUN, units) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN, units) -> output
	}
	return_metric(eventlog, output, level, append, "idle_time", 1)
}
