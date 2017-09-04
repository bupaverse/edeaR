#' @title Metric: Activity Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of traces, cases, resources or activity types.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: log, trace, case, activity.
#'
#' @export activity_frequency

activity_frequency <- function(eventlog,
									level_of_analysis = c("log","trace","activity","case")) {
	stop_eventlog(eventlog)

	level_of_analysis <- match.arg(level_of_analysis)


	FUN <- switch(level_of_analysis,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)

	mapping <- mapping(eventlog)

	if("grouped_eventlog" %in% class(eventlog)) {
		if(level_of_analysis != "log") {
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

	class(output) <- c("activity_frequency", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}
