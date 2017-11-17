#' @title Metric: Resource frequency
#'
#' @description Analyses the frequency of resources at different levels of analysis
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level At which level the analysis of  coverage should be performed: log, case, activity, resource, resource-activity.
#' @param append Logical indicated whether to append result to orignal data.frame
#' @param ... Deprecated arguments
#'
#' @export resource_frequency


resource_frequency <- function(eventlog, level, append, ...) {
	UseMethod("resource_frequency")
}

#' @describeIn resource_frequency Resource frequency for eventlog
#' @export


resource_frequency.eventlog <- function(eventlog,
							   level = c("log","case","activity","resource","resource-activity"),
							   append = F,
							   ...) {

	mapping <- mapping(eventlog)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)

	output <- FUN(eventlog = eventlog)


	return_metric(eventlog, output, level, append, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))
}


#' @describeIn resource_frequency Resource frequency for grouped eventlog
#' @export

resource_frequency.grouped_eventlog <- function(eventlog,
							   level = c("log","case","activity","resource","resource-activity"),
							   append = F,
							   ...) {

	mapping <- mapping(eventlog)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)



		if(!(level %in% c("log"))) {
			grouped_metric(eventlog, FUN) -> output
		}
		else {
			grouped_metric_raw_log(eventlog, FUN)  -> output
		}



	return_metric(eventlog, output, level, append, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))
}
