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
						   sort = TRUE) {
	UseMethod("trace_coverage")
}

#' @describeIn trace_coverage Calculates trace coverage metric for a \code{\link[bupaR]{log}}.
#' @export
trace_coverage.log <- function(log,
							   level = c("log", "trace", "case"),
							   sort = TRUE) {

	level <- rlang::arg_match(level)
	absolute <- NULL


	FUN <- switch(level,
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

	output <- FUN(log = log)

	if(sort && level %in% c("trace", "case")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric_v2(log, output, level, "trace_coverage")
}

#' @describeIn trace_coverage Calculates trace coverage metric for a \code{\link[bupaR]{grouped_log}}.
#' @export
trace_coverage.grouped_log <- function(log,
									   level = c("log", "trace", "case"),
									   sort = TRUE) {

	level <- rlang::arg_match(level)

	absolute <- NULL

	FUN <- switch(level,
				  log = trace_coverage_log,
				  case = trace_coverage_case,
				  trace = trace_coverage_trace)

	output <- apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(sort && level %in% c("trace", "case")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric_v2(log, output, level, "trace_coverage")
}

