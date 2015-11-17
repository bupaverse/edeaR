#' @title n_activity_instances
#'
#' @export n_activity_instances

n_activity_instances <- function(eventlog) {
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"
	return(length(unique(eventlog$activity_instance_classifier)))
}
