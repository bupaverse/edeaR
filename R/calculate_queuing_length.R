#' @title Calculate queuing length
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param queueing_times Object of class \code{queuing_times}, returned by \code{\link{calculate_queuing_times}}.
#' @param level \code{\link{character}} (default "log"): Level of granularity for the analysis: \code{"log"}, \code{"activity"}, \code{"resource"}. For more information, see 'Details' below.
#' @param time_interval The time interval after which the queue length should be calculated. For more information, see 'Details' below and the \code{by} argument of \code{\link{seq.Date}}.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{log} level, this metric calculates the total number of activity instances that are queued at a given moment in time.
#' \item At \code{resource} level, this metric calculates the total number activity instances that are queued for a given resource.
#' \item On \code{activity} level, this metric calculates the total number of activity instances that are queue for a given activity type.
#' }
#' Argument \code{time_interval} has the following options (see also the \code{by} argument of \code{\link{seq.Date}}):
#' \itemize{
#' \item A \code{\link{numeric}} as number of days.
#' \item An object of class \code{\link{difftime}}.
#' \item A \code{\link{character}} string, which could be one of \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' The first day for which queue length is calculated, is the first timestamp found in the log.
#' }
#'
#' @seealso \code{\link{calculate_queuing_times}}, \code{\link{seq.Date}}
#'
#' @concept queues
#'
#' @export
calculate_queuing_length <- function(queueing_times, level = c("log", "activity", "resource"), time_interval) {
	UseMethod("calculate_queuing_length")
}

#' @export
calculate_queuing_length.queuing_times <- function(queueing_times,
												   level = c("log", "activity", "resource"),
												   time_interval) {

	n_queued <- NULL
	level <- rlang::arg_match(level)

	start_date <- as.Date(min(queueing_times$in_queue_since))
	end_date <- as.Date(max(queueing_times$started))

	tibble(date = seq.Date(start_date, end_date, by = time_interval)) %>%
		mutate(n_queued = map(date, compute_nr_queued, queue = queueing_times, level = level)) %>%
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
