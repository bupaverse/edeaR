#' Metric: Trace coverage
#'
#'
#' Analyses the structuredness of an event log by use of trace frequencies. Applicable at logn case and trace level
#'
#'
#'
#' \itemize{
#' \item Trace: The absolute and relative frequency of each trace is returned
#'
#' \item Case: for each case, the coverage of the corresponding trace is returned
#'
#' \item Log: Summary statistics of the coverage of traces is returned.
#' }

#'
#' @inherit throughput_time params
#' @inherit activity_frequency params references seealso return
#'
#' @export trace_coverage

trace_coverage <- function(eventlog, level, append,  ...) {
	UseMethod("trace_coverage")
}


#' @describeIn trace_coverage Trace coverage metric for eventlog
#' @export

trace_coverage.eventlog <- function(eventlog, level = c("log","trace","case"), append = F, ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	if(exists("threshold")) {
		warning("The threshold parameter is no longer supported")
	}

	FUN <- switch(level,
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

		output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "trace_coverage", 2)

}


#' @describeIn trace_coverage Trace coverage metric for grouped eventlog
#' @export

trace_coverage.grouped_eventlog <- function(eventlog, level = c("log","trace","case"), append = F, ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	if(exists("threshold")) {
		warning("The threshold parameter is no longer supported")
	}

	FUN <- switch(level,
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

	output <- grouped_metric(eventlog, FUN)


	return_metric(eventlog, output, level, append, "trace_coverage", 2)

}

