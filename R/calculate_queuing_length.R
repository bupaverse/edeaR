



#' Calculate queue length
#'
#' @param queueing_times A data frame with queueing times, returned by calculating_queuing_times
#' @param level The level of granularity (log, activity or resource). Log: total number of activity instances  that are queued at a given moment in time. Resource: total number activity instances  that are queued for a given resource. Activity: total number of activity instances that are queue for a given activity type.
#' @param time_interval The time interval after which the queue length should be calculated. Can be day, week, month, year, or a number of days. The first day for which queue length is calculated is the first timestamp found in the log.
#'
#' @export
#'
calculate_queuing_length <- function(queueing_times, level, time_interval) {
	UseMethod("calculate_queuing_length")
}

#' @export
calculate_queuing_length.queuing_times <- function(queueing_times, level = c("log","activity","resource"),
												   time_interval) {

	n_queued <- NULL

	start_date = as.Date(min(queueing_times$in_queue_since))
	end_date = as.Date(max(queueing_times$started))

	level <- match.arg(level)

	tibble(date = seq.Date(start_date, end_date, by = time_interval)) %>%
		mutate(n_queued = map(date,compute_nr_queued, queue = queueing_times, level = level)) %>%
		unnest(n_queued) %>%
		rename(queue_length = n)
}


compute_nr_queued <- function(queue, date, level) {
	mapping <- attr(queue, "mapping")

	in_queue_since <- NULL
	started <- NULL

	if(level == "log") {
		queue %>%
			filter(in_queue_since < date, started > date) %>%
			count()
	} else if(level == "activity") {
		queue %>%
			filter(in_queue_since < date, started > date) %>%
			count(!!activity_id_(mapping))
	} else if(level == "resource") {
		queue %>%
			filter(in_queue_since < date, started > date) %>%
			count(!!resource_id_(mapping))
	}

}
