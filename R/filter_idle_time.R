#' @title Filter Idle Time
#'
#' @description Filters cases based on their \code{\link{idle_time}}.
#'
#' This filter can be used by using an \code{interval} or by using a \code{percentage}.
#' The percentage will always start with the cases with the lowest idle time first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have an idle time in this interval. The time units
#' in which this interval is defined can be supplied with the \code{units} argument.
#'
#' @param interval,percentage Provide either \code{interval} or \code{percentage}.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): A duration interval. Half open interval can be created using \code{\link{NA}}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentage to be used for relative filtering.
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_throughput_time params
#'
#' @seealso \code{\link{idle_time}}
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

#' @describeIn filter_idle_time Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_idle_time.log <- function(log,
									   interval = NULL,
									   percentage = NULL,
									   reverse = FALSE,
									   units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = T) || all(is.na(interval)) )) {
		stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
	}
	if(!is.null(percentage) && (!is.numeric(percentage) || !between(percentage,0,1) )) {
		stop("Percentage should be a numeric value between 0 and 1.")
	}

	if(is.null(interval) & is.null(percentage))
		stop("At least an interval or a percentage must be provided.")
	else if((!is.null(interval)) & !is.null(percentage))
		stop("Cannot filter on both interval and percentage simultaneously.")
	else if(!is.null(percentage))
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

#' @describeIn filter_idle_time Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_idle_time.grouped_log <- function(log,
											   interval = NULL,
											   percentage = NULL,
											   reverse = FALSE,
											   units = c("secs", "mins", "hours", "days", "weeks")) {

	bupaR:::apply_grouped_fun(log, fun = filter_idle_time.log, interval, percentage, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_processing_time, interval = interval, percentage = percentage, reverse, units, ...)
}
