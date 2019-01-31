#' Metric: Trace length
#'
#'
#' Analysis of trace lengths
#'
#' This metric provides an overview of the number of activities that occur in each trace.
#'  In this metric, instances of an activity, as
#'  opposed to the actual activities, are calculated.
#'
#'  \itemize{
#'
#'  \item On the level of the log, the number of actual
#'  transactions in a trace are calculated and aggregated on the log level.
#'
#'  \item On the level of the cases, this metric calculates the number of activity instances in each case.
#'
#'  \item This metric shows the number of activity instances
#'  executed in each trace.
#'  #'
#'  }
#'
#' @inherit throughput_time params
#' @inherit activity_frequency params references seealso return

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
								  append_column = NULL,
								  sort = TRUE,
								  ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "trace_length",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	output <- FUN(eventlog = eventlog)

	if(sort && level %in% c("trace","case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(eventlog, output, level, append, append_column,  "trace_length", 1,  empty_label = T)

}


#' @describeIn trace_length Trace length for grouped eventlog
#' @export

trace_length.grouped_eventlog <- function(eventlog,
										  level = c("log","trace","case"),
										  append = F,
										  append_column = NULL,
										  sort = TRUE,
										  ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "trace_length",
								   T ~ "NA")
	}

	if(!(level %in% c("log"))) {
		output <- grouped_metric(eventlog, FUN)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN)
	}

	if(sort && level %in% c("trace","case")) {
		output %>%
			arrange(-absolute) -> output
	}


	return_metric(eventlog, output, level, append, append_column, "trace_length", 1, empty_label = T)
}
