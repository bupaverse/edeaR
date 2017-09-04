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
						 level_of_analysis = c("log","trace","case")) {

	stop_eventlog(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)
	mapping <- mapping(eventlog)

	FUN <- switch(level_of_analysis,
				  log = trace_length_log,
				  case = trace_length_case,
				  trace = trace_length_trace)

	if("grouped_eventlog" %in% class(eventlog)) {
		if(!(level_of_analysis %in% c("log"))) {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN)) %>%
				unnest -> output
		}
		else {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN)) -> temp

			temp %>%
				mutate(raw = map(data, attr, "raw")) %>%
				select(-data) %>%
				unnest() -> raw

			temp %>%
				mutate(data = map(data, ~as.data.frame(as.list(.x)))) %>%
				unnest() -> output

			attr(output, "raw") <- raw
		}

		attr(output, "groups") <- groups(eventlog)
	}
	else{
		output <- FUN(eventlog = eventlog)
	}

	class(output) <- c("trace_length", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)

}
