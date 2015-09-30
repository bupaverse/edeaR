
#' @title Metric:  Repetitions
#'
#' @description  Provides summuary statistics on the number of repetitions, at the level of activity types, traces and the eventlog.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of repetitions should be performed: trace or activity.
#'
#'
#' @export repetitions


repetitions <- function(eventlog,
						level_of_analysis){

	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("log","trace","activity")))
		stop("Level of analysis should be one of the following: log, trace, activity.")

	if(level_of_analysis == "log"){
		return(repetitions_log(eventlog = eventlog))
	}
	else if(level_of_analysis == "trace") {
		return(repetitions_trace(eventlog = eventlog))
	}
	else
		return(repetitions_activity(eventlog = eventlog))
}
