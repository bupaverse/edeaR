#' @title Metric: Activity Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of traces, cases, resources or activity types.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level At which level the analysis of activity type frequency should be performed: log, trace, case, activity.
#' @param append Logical, indicating whether to append results to original event log. Ignored when level is log or trace.
#' @param ... Deprecated arguments
#'
#' @export activity_frequency

activity_frequency <- function(eventlog, level,  append, ...) {
	UseMethod("activity_frequency")
}


#' @describeIn activity_frequency Compute activity frequency for eventlog
#' @export

activity_frequency.eventlog <- function(eventlog,
										level = c("log","trace","activity","case"),
										append = F,
										...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)


	output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "activity_frequency")

}


#' @describeIn activity_frequency Compute activity frequency for grouped event log
#' @export

activity_frequency.grouped_eventlog <- function(eventlog,
												level = c("log","trace","activity","case"),
												append = F,
												...) {
	level <- match.arg(level)

	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)

	if(level != "log") {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}

	return_metric(eventlog, output, level, append, "activity_frequency")


}
