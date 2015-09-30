#' @title Metric: Size of selfloops
#'
#' @description Provides summary statistics on the sizes of selfloops at the level of activity types or traces. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: trace or activity.
#'
#' @param include_non_selfloops \code{Logical}. When true, also singular events, i.e. selfloops of size 1, are considered.
#'
#' @export size_of_selfloops

size_of_selfloops <- function(eventlog,
							  level_of_analysis,
							  include_non_selfloops = FALSE){

	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("trace","activity", "log")))
		stop("Level of analysis should be one of the following: log, trace, activity.")


	if(level_of_analysis == "trace") {
		return(size_of_selfloops_trace(eventlog = eventlog, include_non_selfloops = include_non_selfloops))
	}
	else if(level_of_analysis == "activity"){
		return(size_of_selfloops_activity(eventlog = eventlog, include_non_selfloops = include_non_selfloops))
	}
	else {
		return(size_of_selfloops_log(eventlog = eventlog, include_non_selfloops = include_non_selfloops))
	}
}
