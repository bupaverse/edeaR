#' @title Metric: Trace length
#'
#' @description Computes the length of each trace, in terms of the number of events, at the level of the eventlog or the level of a trace.
#' The relative numbers at trace level measure trace length compared to the average trace length of the top 80% cases, approximately.
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#' @param level_of_analysis At which level the analysis of  trace_length should be performed: log, case or trace.

#' @export trace_length

trace_length <- function(eventlog,
						 level_of_analysis = c("trace","case","log")) {

	stop_eventlog(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)

	if(level_of_analysis == "trace")
		output <- trace_length_trace(eventlog = eventlog)
	else if (level_of_analysis == "case")
		output <- trace_length_case(eventlog = eventlog)
	else
		output <- trace_length_log(eventlog = eventlog)


	class(output) <- c("trace_length", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)

}
