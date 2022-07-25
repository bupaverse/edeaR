#' @title Calculate queuing times
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param units \code{\link{character}} (default \code{"auto"}): The time unit in which the throughput times should be reported. Should be one of the following values:
#' \code{"auto"} (default), \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}, \code{"weeks"}. See also the \code{units} argument of \code{\link{difftime}}.
#'
#' @return Returns a list of all the activity instances, with the time they started, and the time since they were queued. Notice that this does not take into account
#' any process model notion! The time since they are queued is the completion time of the previous activity in the log.
#'
#' @inherit activity_frequency params references
#'
#' @seealso \code{\link{difftime}}
#'
#' @export
calculate_queuing_times <- function(log, units = c("auto", "secs", "mins", "hours", "days", "weeks"), eventlog = deprecated()) {
	UseMethod("calculate_queuing_times")
}

#' @describeIn calculate_queuing_times Calculate queueing times for \code{\link[bupaR]{eventlog}} and \code{\link[bupaR]{grouped_eventlog}}.
#' @export
calculate_queuing_times.eventlog <- function(log, units = c("auto", "secs", "mins", "hours", "days", "weeks"), eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	units <- rlang::arg_match(units)

	log %>%
		group_by(.data[[case_id(.)]],
				 .data[[activity_id(.)]],
				 .data[[activity_instance_id(.)]],
				 .data[[resource_id(.)]]) %>%
		summarize("started" = min(.data[[timestamp(log)]]),
				  "e" = max(.data[[timestamp(log)]])) %>%
		group_by(.data[[case_id(log)]]) %>%
		arrange(.data[["started"]]) %>%
		mutate("in_queue_since" = lag(.data[["e"]])) %>%
		filter(!is.na(.data[["in_queue_since"]])) %>%
		#rename(started = s) %>%
		select(1:4, .data[["started"]], .data[["in_queue_since"]]) %>%
		mutate("time_in_queue" = difftime(.data[["started"]], .data[["in_queue_since"]], units = units)) %>%
		ungroup() -> output

	class(output) <- c("queuing_times", class(output))
	attr(output, "mapping") <- mapping(log)
	return(output)
}

#' @describeIn calculate_queuing_times Calculate queueing times for \code{\link[bupaR]{activitylog}} and \code{\link[bupaR]{grouped_activitylog}}.
#' @export
calculate_queuing_times.activitylog <- function(log, units = c("auto", "secs", "mins", "hours", "days", "weeks"), eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	units <- rlang::arg_match(units)

	calculate_queuing_times.eventlog(bupaR::to_eventlog(log), units = units)
}