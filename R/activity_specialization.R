#' @title Metric: Activity Specialization
#'
#' @description Analyses whether activities are specialized in by specific resources
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of coverage should be performed: case, activity
#'
#' @export activity_specialization


activity_specialization <- function(eventlog, level_of_analysis) {
	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("case", "activity")))
		stop("Level of analysis should be one of the following: case, activity")

	if(level_of_analysis == "case")
		return(activity_specialization_case(eventlog = eventlog))
	else
		return(activity_specialization_activity(eventlog = eventlog))

}
