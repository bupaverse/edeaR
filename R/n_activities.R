#' @title n_activities
#'
#' @export n_activities

n_activities <- function(eventlog){
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	return(length(unique(eventlog$event_classifier)))
}
