#' @title Metric: Idle Time
#'
#' @description Calculates the amount of time that no activity occurs for a case or for a resource. At log level it gives summary statistics of all cases in the log. At trace level it provides summary statistics of all cases related to this case.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: log, trace, case, resource.
#'
#' @export idle_time

idle_time <- function(eventlog,
							   level_of_analysis,
							  	units = "days") {
	stop_eventlog(eventlog)

	if(!(level_of_analysis %in% c("trace", "resource","case", "log")))
		stop("Level of analysis should be one of the following: log,  trace, case, resource")

	if (level_of_analysis == "trace")
		return(idle_time_trace(eventlog, units = units))
	else if (level_of_analysis == "case")
		return(idle_time_case(eventlog, units = units))
	else if(level_of_analysis == "resource")
		return(idle_time_resource(eventlog, units = units))
	else if (level_of_analysis == "log")
		return(idle_time_log(eventlog, units = units))
}
