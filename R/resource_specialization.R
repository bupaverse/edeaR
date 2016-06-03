#' @title Metric: Resource Specialization
#'
#' @description Analyses whether resources specialize in specific activities
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case, resource.
#'
#' @export resource_specialization


resource_specialization <- function(eventlog, level_of_analysis) {
	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("log","case", "resource")))
		stop("Level of analysis should be one of the following: log, case, resource")

	if(level_of_analysis == "log")
		return(resource_specialization_log(eventlog = eventlog))
	else if(level_of_analysis == "case")
		return(resource_specialization_case(eventlog = eventlog))
	else
		return(resource_specialization_resource(eventlog = eventlog))

}
