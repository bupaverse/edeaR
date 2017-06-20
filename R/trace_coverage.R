#' @title Metric: Trace coverage
#'
#' @description Analyses the structuredness of an event log by use of trace frequencies. Applicable at logn case and trace level
#'
#' Trace: The absolute and relative frequency of each trace is returned
#'
#' Case: for each case, the coverage of the corresponding trace is returned
#'
#' Log: The number of traces to cover a certain percentage (default is 80\%) of a log is computed. If a tie exists, the two nearest points are returned.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case or trace.
#'
#' @param threshold The threshold to be used for the analysis at log level. Default is at 0.8 (80\%)
#'
#' @export trace_coverage


trace_coverage <- function(eventlog, level_of_analysis = c("log","trace","case"), threshold = NULL) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)

	if(level_of_analysis == "trace")
		output <- trace_coverage_trace(eventlog = eventlog)
	else if(level_of_analysis == "case")
		output <- trace_coverage_case(eventlog = eventlog)
	else
		output <- trace_coverage_log(eventlog = eventlog, threshold = threshold)


	class(output) <- c("trace_coverage", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}
