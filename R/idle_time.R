#' @title Metric: Idle Time
#'
#' @description Calculates the amount of time that no activity occurs for a case or for a resource. At log level it gives summary statistics of all cases in the log. At trace level it provides summary statistics of all cases related to this case.
#''
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of activity type frequency should be performed: log, trace, case, resource.
#'
#' @param units Time units to be used
#'
#' @export idle_time

idle_time <- function(eventlog,
							   level_of_analysis = c("trace","resource","case","log"),
							  	units = c("hours","days", "weeks","mins")) {
	stop_eventlog(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)
	units <- match.arg(units)

	if (level_of_analysis == "trace")
		output <- idle_time_trace(eventlog, units = units)
	else if (level_of_analysis == "case")
		output <- idle_time_case(eventlog, units = units)
	else if(level_of_analysis == "resource")
		output <- idle_time_resource(eventlog, units = units)
	else if (level_of_analysis == "log")
		output <- idle_time_log(eventlog, units = units)


	class(output) <- c("idle_time", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)
	attr(output, "units") <- units

	return(output)
}
