#' @title Metric: Throughput time of cases
#'
#' @description  Provides summary statistics concerning the throughput times of cases.
#' The throughput time of cases is defined as the time between the start of the first event and the completion of the last event.
#' Can be performed at the level of the log as well as the level of traces and cases.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#' @param level_of_analysis At which level the analysis of throughput times should be performed: log, case or trace.
#'
#'
#' @export throughput_time
#'

throughput_time <- function(eventlog,
							level_of_analysis = c("log","trace","case"),
							units = c("days", "hours","mins","weeks")){

	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)
	units <- match.arg(units)


	FUN <- switch(level_of_analysis,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)

	mapping <- mapping(eventlog)

	if("grouped_eventlog" %in% class(eventlog)) {
		if(level_of_analysis != "log") {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN, units = units)) %>%
				unnest -> output
		}
		else {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN, units = units)) -> temp

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
		output <- FUN(eventlog = eventlog, units = units)
	}



	class(output) <- c("throughput_time", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)
}
