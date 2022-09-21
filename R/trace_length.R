#' @title Trace Length
#'
#' @description Analysis of trace lengths
#'
#' This metric provides an overview of the number of activities that occur in each trace.
#'
#' An important remark is that this metric takes into account each instance of an activity, but not the individual lifecycle events.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, the summary statistics describing the trace length of cases in an aggregated fashion.
#' \item On \code{"trace"} level, the trace length of the different process variants or traces in the log are calculated.
#' \item On \code{"case"} level, the trace lengths for each case are computed.
#' }
#'
#' @inherit throughput_time params
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export trace_length
trace_length <- function(log,
						 level = c("log", "trace", "case"),
						 append = deprecated(),
						 append_column = NULL,
						 sort = TRUE,
						 eventlog = deprecated()) {
	UseMethod("trace_length")
}

#' @describeIn trace_length Computes trace length for an \code{\link[bupaR]{eventlog}}.
#' @export
trace_length.eventlog <- function(log,
								  level = c("log", "trace", "case"),
								  append = deprecated(),
								  append_column = NULL,
								  sort = TRUE,
								  eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "trace_length(eventlog)",
			with = "trace_length(log)")
		log <- eventlog
	}
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	output <- FUN(log = log)

	if(sort && level %in% c("trace", "case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column,  "trace_length", 1,  empty_label = TRUE)
}

#' @describeIn trace_length Computes trace length for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
trace_length.grouped_eventlog <- function(log,
										  level = c("log", "trace", "case"),
										  append = deprecated(),
										  append_column = NULL,
										  sort = TRUE,
										  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	FUN <- switch(level,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "trace_length",
								   T ~ "NA")
	}

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	#if(!(level %in% c("log"))) {
	#	output <- grouped_metric(eventlog, FUN)
	#}
	#else {
	#	output <- grouped_metric_raw_log(eventlog, FUN)
	#}

	if(sort && level %in% c("trace","case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "trace_length", 1, empty_label = TRUE)
}

#' @describeIn trace_length Computes trace length for an \code{\link[bupaR]{activitylog}}.
#' @export
trace_length.activitylog <- function(log,
									 level = c("log", "trace", "case"),
									 append = deprecated(),
									 append_column = NULL,
									 sort = TRUE,
									 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	trace_length.eventlog(bupaR::to_eventlog(log),
						  level = level,
						  append = append,
						  append_column = append_column,
						  sort = sort)
}

#' @describeIn trace_length Computes trace length for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
trace_length.grouped_activitylog <- function(log,
											 level = c("log", "trace", "case"),
											 append = deprecated(),
											 append_column = NULL,
											 sort = TRUE,
											 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	trace_length.grouped_eventlog(bupaR::to_eventlog(log),
								  level = level,
								  append = append,
								  append_column = append_column,
								  sort = sort)
}