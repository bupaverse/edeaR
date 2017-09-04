#' @title Metric: Resource frequency
#'
#' @description Analyses the frequency of resources at different levels of analysis
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case, activity, resource, resource-activity.
#'
#'
#' @export resource_frequency


resource_frequency <- function(eventlog, level_of_analysis = c("log","case","activity","resource","resource-activity")) {
	stop_eventlog(eventlog)
	mapping <- mapping(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)

	FUN <- switch(level_of_analysis,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)



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

	class(output) <- c("resource_frequency", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)




}
