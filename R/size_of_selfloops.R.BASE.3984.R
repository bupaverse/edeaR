#' @title Metric: Size of selfloops
#'
#' @description Provides summary statistics on the sizes of selfloops at the level of activity types, cases, traces or log. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type Type of selfloops. I.e. repeat or redo
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: log, case, activity, resource, resource activity.
#'
#'
#' @export size_of_selfloops

size_of_selfloops <- function(eventlog,
							  type = c("repeat","redo"),
							  level_of_analysis = c("log","case","activity","resource","resource-acitivty")){


	stop_eventlog(eventlog)
	type <- match.arg(type)
	level_of_analysis <- match.arg(level_of_analysis)
	mapping <- mapping(eventlog)

	if(type == "repeat") {
		FUN <- switch(level_of_analysis,
			   log = repeat_selfloops_size_log,
			   case = repeat_selfloops_size_case,
			   activity = repeat_selfloops_size_activity,
			   resource = repeat_selfloops_size_resource,
			   "resource-activity" = repeat_selfloops_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level_of_analysis,
			   log = redo_selfloops_size_log,
			   case = redo_selfloops_size_case,
			   activity = redo_selfloops_size_activity,
			   resource = redo_selfloops_size_resource,
			   "resource-activity" = redo_selfloops_size_resource_activity
		)

	}

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

			# temp %>%
			# 	mutate(raw = map(data, attr, "raw")) %>%
			# 	select(-data) %>%
			# 	unnest() -> raw

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

	class(output) <- c("size_of_selfloops", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "type") <- type
	return(output)
}
