#' @title Mapping
#'
#' @description Prints the mapping of an event log object.
#'
#' @export mapping

mapping <- function(eventlog) {
	stop_eventlog(eventlog)
	message(paste0("Case identifier:\t\t", case_id(eventlog)))
	message(paste0("Activity identifier:\t\t", activity_id(eventlog)))
	message(paste0("Activity instance identifier:\t", activity_instance_id(eventlog)))
	message(paste0("Timestamp:\t\t\t", timestamp(eventlog)))
	message(paste0("Lifecycle transition:\t\t", lifecycle_id(eventlog = eventlog)))
}
