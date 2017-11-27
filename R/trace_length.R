#' @title Metric: Trace length
#'
#' @description Computes the length of each trace, in terms of the number of events, at the level of the eventlog or the level of a trace.
#' The relative numbers at trace level measure trace length compared to the average trace length of the top 80% cases, approximately.
#'
#' @inheritParams activity_frequency

#' @export trace_length

trace_length <- function(eventlog, level, append, ...) {
	UseMethod("trace_length")
}


#' @describeIn trace_length Trace length for  eventlog
#' @export
#'
trace_length.eventlog <- function(eventlog,
								  level = c("log","trace","case"),
								  append = F,
								  ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "trace_length", 1)

}


#' @describeIn trace_length Trace length for grouped eventlog
#' @export

trace_length.grouped_eventlog <- function(eventlog,
										  level = c("log","trace","case"),
										  append = F,
										  ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	if(!(level %in% c("log"))) {
		output <- grouped_metric(eventlog, FUN)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN)
	}

	return_metric(eventlog, output, level, append, "trace_length", 1)
}
