#'  Metric: Idle Time
#'
#'  Calculates the amount of time that no activity occurs.
#'
#' \itemize{
#'
#' \item  On the level of the complete event log, the
#'   idle time metric provides an overview of summary statistics of the idle
#'   time per case, aggregated over the complete event log.

#'\item  The metric applied on the level of the specific
#'  cases in the event log provides an overview of the total idle time per case
#'
#' \item On the level of the different traces that occur in the event log,
#'  the idle time metric provides an overview of the summary statistics
#'  of the idle time for each trace in the event log.
#'
#' \item The metric can also be of interest on the level of the resources,
#'  to get an insight in the amount of time each resource \"wastes\" during the process.
#' }
#'
#' @param level Level of granularity for the analysis: log,  case, trace, or resource.
#' For more information, see \code{vignette("metrics", "edeaR")}
#'
#' @param units Time units to be used
#'
#' @inherit activity_frequency params references seealso return
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
							   units = c("hours","days","weeks","mins", "secs"),
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
