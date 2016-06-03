#' @title Filter: Activity
#'
#' @description Filters the log based on activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param activities A vector of activities to withhold
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_activity
#'
filter_activity <- function(eventlog,
									  activities = NULL,
									  reverse = F){
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"


	if(reverse == F)
		output <- filter(eventlog, event_classifier %in% activities)

	else
		output <- filter(eventlog, !(event_classifier %in% activities))

	colnames(output)[colnames(output)=="event_classifier"] <- activity_id(eventlog)

	output <- eventlog(output,
					   activity_id = activity_id(eventlog),
					   case_id = case_id(eventlog),
					   timestamp =timestamp(eventlog),
					   lifecycle_id = lifecycle_id(eventlog),
					   activity_instance_id = activity_instance_id(eventlog))

	return(output)
}
