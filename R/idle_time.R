#' @title Metric: Idle Time
#'
#' @description Calculates the amount of time that no activity occurs for a case or for a resource. At log level it gives summary statistics of all cases in the log. At trace level it provides summary statistics of all cases related to this case.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: log, trace, case, resource.
#'
#' @param units Time units to be used
#'
#' @export idle_time

idle_time <- function(eventlog,
							   level_of_analysis = c("log","case","trace","resource"),
							  	units = c("hours","days", "weeks","mins")) {
	stop_eventlog(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)
	units <- match.arg(units)
	mapping <- mapping(eventlog)

	FUN <- switch(level_of_analysis,
				  log = idle_time_log,
				  case = idle_time_case,
				  trace = idle_time_trace,
				  resource = idle_time_resource)

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

	class(output) <- c("idle_time", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)
}
