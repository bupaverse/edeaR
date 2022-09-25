#' @title Trace Coverage
#'
#' @description Analyses the structuredness of a log by use of trace frequencies.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, summary statistics of the coverage of traces are returned.
#' \item On \code{"trace"} level, the absolute and relative frequency of each trace are returned.
#' \item On \code{"case"} level, the coverage of the corresponding trace is returned for each case.
#' }
#'
#' @inherit throughput_time params
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export trace_coverage
trace_coverage <- function(log,
						   level = c("log", "trace", "case"),
						   append = deprecated(),
						   append_column = NULL,
						   sort = TRUE,
						   eventlog = deprecated()) {
	UseMethod("trace_coverage")
}

#' @describeIn trace_coverage Calculates trace coverage metric for a \code{\link[bupaR]{log}}.
#' @export
trace_coverage.log <- function(log,
							   level = c("log", "trace", "case"),
							   append = deprecated(),
							   append_column = NULL,
							   sort = TRUE,
							   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "number_of_repetitions(eventlog)",
			with = "number_of_repetitions(log)")
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
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

	output <- FUN(log = log)

	if(sort && level %in% c("trace", "case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "trace_coverage", 2)
}

#' @describeIn trace_coverage Calculates trace coverage metric for a \code{\link[bupaR]{grouped_log}}.
#' @export
trace_coverage.grouped_log <- function(log,
									   level = c("log", "trace", "case"),
									   append = deprecated(),
									   append_column = NULL,
									   sort = TRUE,
									   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(sort && level %in% c("trace", "case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "trace_coverage", 2)
}

