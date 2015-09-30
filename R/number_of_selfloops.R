#' @title Metric: Number of selfloops in trace
#'
#' @description Returns the number of selfloops in each trace. Can be performed at the level of traces, activities, or the level of the event log.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: trace or log
#'
#'
#' @export number_of_selfloops


number_of_selfloops <- function(eventlog,
								level_of_analysis) {

	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("activity", "trace","log")))
		stop("Level of analysis should be one of the following: activity, trace, log.")

	if(level_of_analysis == "trace") {
		return(number_of_selfloops_trace(eventlog = eventlog))
	}
	else if (level_of_analysis == "activity")
		return(number_of_selfloops_activity(eventlog = eventlog))
	else
		return(number_of_selfloops_log(eventlog = eventlog))
}
