

#' Calculate queuing times
#'
#' @param eventlog The event log object
#'
#' @return Returns a list of all the activity instances, with the time they started, and the time since they were queued. Notice that this does not take into account
#' any process model notion! The time since they are queued is the completion time of the previous activity in the log.
#' @export
#'
calculate_queuing_times <- function(eventlog) {
	UseMethod("calculate_queuing_times")
}
#'
#'
#' @export
calculate_queuing_times.eventlog <- function(eventlog) {

	s <- NULL
	e <- NULL
	in_queue_since <- NULL
	started <- NULL

	eventlog %>%
		group_by(!!case_id_(eventlog),
				 !!activity_id_(eventlog),
				 !!activity_instance_id_(eventlog),
				 !!resource_id_(eventlog)) %>%
		summarize(s = min(!!timestamp_(eventlog)), e = max(!!timestamp_(eventlog))) %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(s) %>%
		mutate(in_queue_since = lag(e)) %>%
		filter(!is.na(in_queue_since)) %>%
		rename(started = s) %>%
		select(1:4, started, in_queue_since) %>%
		mutate(time_in_queue = as.double(started - in_queue_since, units = "hours")) %>%
		ungroup() -> output

	class(output) <- c("queuing_times", class(output))
	attr(output, "mapping") <- mapping(eventlog)
	return(output)
}
