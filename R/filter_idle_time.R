#' @title Filter Idle Time
#'
#' @description Filters cases based on their [`idle_time`].
#'
#' This filter can be used by using an `interval` or by using a `percentage`.
#' The percentage will always start with the cases with the lowest idle time first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have an idle time in this interval. The time units
#' in which this interval is defined can be supplied with the `units` argument.
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_processing_time params
#'
#' @seealso [`idle_time()`],[`difftime()`]
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_idle_time
filter_idle_time <- function(log,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   units = c("secs", "mins", "hours", "days", "weeks")) {
	UseMethod("filter_idle_time")
}

#' @describeIn filter_idle_time Filters cases for a [`log`][`bupaR::log`].
#' @export
filter_idle_time.log <- function(log,
									   interval = NULL,
									   percentage = NULL,
									   reverse = FALSE,
									   units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	check_interval_percentage_args(interval, percentage)

	if(!is.null(percentage))
		filter_idle_time_percentile(log,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_idle_time_threshold(log,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse,
										 units = units)
}

#' @describeIn filter_idle_time Filters cases for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
filter_idle_time.grouped_log <- function(log,
											   interval = NULL,
											   percentage = NULL,
											   reverse = FALSE,
											   units = c("secs", "mins", "hours", "days", "weeks")) {

	bupaR:::apply_grouped_fun(log, fun = filter_idle_time.log, interval, percentage, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_processing_time, interval = interval, percentage = percentage, reverse, units, ...)
}
