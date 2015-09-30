#' @title Metric: Throughput time of cases
#'
#' @description  Provides summary statistics concerning the throughput times of cases.
#' The throughput time of cases is defined as the time between the start of the first event and the completion of the last event.
#' Can be performed at the level of the log as well as the level of traces.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#' @param level_of_analysis At which level the analysis of throughput times should be performed: log or trace.
#'
#'
#' @export throughput_time
#'

throughput_time <- function(eventlog,
							units = "days",
							level_of_analysis){

	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("log","trace")))
		stop("Level of analysis should be one of the following: log, trace.")

	if(level_of_analysis == "trace")
		return(throughput_time_trace(eventlog = eventlog, units = units))
	else
		return(throughput_time_log(eventlog = eventlog, units = units))

}
