
#' @title Metric:  Number of repetitions
#'
#' @description  Provides summuary statistics on the number of repetitions, at the level of activity types, traces, cases and the eventlog.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type The type of repetitions, either repeat or redo
#'
#' @param level_of_analysis At which level the analysis of repetitions should be performed: log, case, activity, resource, resource-activity.
#'
#'
#' @export number_of_repetitions


number_of_repetitions <- function(eventlog,
								  type = c("repeat","redo"),
						level_of_analysis = c("log","case","activity","resource","resource-activity")){

	stop_eventlog(eventlog)
	mapping <- mapping(eventlog)
	type <- match.arg(type)
	level_of_analysis <- match.arg(level_of_analysis)

	if(type == "repeat") {
		FUN <- switch(level_of_analysis,
			   log = repeat_repetitions_log,
			   case = repeat_repetitions_case,
			   activity = repeat_repetitions_activity,
			   resource = repeat_repetitions_resource,
			   "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level_of_analysis,
			   log = redo_repetitions_log,
			   case = redo_repetitions_case,
			   activity = redo_repetitions_activity,
			   resource = redo_repetitions_resource,
			   "resource-activity" = redo_repetitions_resource_activity
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

	class(output) <- c("number_of_repetitions", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "type") <- type

	return(output)

}
