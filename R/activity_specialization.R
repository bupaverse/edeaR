#' @title Metric: Activity Specialization
#'
<<<<<<< HEAD
#' @description Analyses whether activities are specialize in by specific resources.
=======
#' @description Analyses whether activities are specialized in by specific resources
>>>>>>> 39b4f132081504572b867c36f5460610261f0b95
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of coverage should be performed: case, activity
#'
#' @return At the activity level, calculated the number of resources which execute an activity in absolute value and relative to the
#' total number of resources.
#'
#' At case level, calculates the number of resources which work on the case in absolute value and relative to the total number of resources.
#'
#'
#'
#' @export activity_specialization


activity_specialization <- function(eventlog, level_of_analysis) {
	stop_eventlog(eventlog)


	if(!(level_of_analysis %in% c("case", "activity")))
		stop("Level of analysis should be one of the following: case, activity", call. = F)

	if(level_of_analysis == "case")
		return(activity_specialization_case(eventlog = eventlog))
	else
		return(activity_specialization_activity(eventlog = eventlog))

}
