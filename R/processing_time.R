#' @title Metric: Processing time
#'
#' @description Provides summary statistics about the processing time of events on the level of activities, traces, cases or log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#' @param level_of_analysis At which level the analysis of processing times should be performed: log, trace, case, resource or activity.
#'#'
#' @export processing_time

processing_time <- function(eventlog,
							level_of_analysis = c("log","trace","case","activity","resource","resource-activity"),
							units = c("hours","days","weeks","mins")){

	level_of_analysis <- match.arg(level_of_analysis)
	units <- match.arg(units)
	stop_eventlog(eventlog)
	mapping <- mapping(eventlog)

	FUN <- switch(level_of_analysis,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	if("grouped_eventlog" %in% class(eventlog)) {
		if(!(level_of_analysis %in% c("log","activity","resource-activity","resource"))) {
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

	class(output) <- c("processing_time", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units


	return(output)

}
