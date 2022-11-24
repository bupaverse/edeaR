#' @title Filter directly follows with time interval
#'
#' @description Filter cases where the activity `from` is followed by activity `to` within a certain time `interval`.
#'
#' @param interval \code{\link{numeric}} vector of length 2): A duration interval. Half open interval can be created using \code{\link{NA}}.
#' @param from,to \code{\link{character}} vector of length one: The antecendent and consequent to filter on. Both are \code{\link{character}} vectors containing exactly one activity identifier.
#' @inherit filter_activity params references seealso return
#' @inherit processing_time params
#'
#' @seealso \code{\link{processing_time}}
#'
#' @family filters
#'
#' @concept filters_case
#' @importFrom data.table `%between%`
#'
#' @export filter_flow_time
filter_flow_time <- function(log,
							 from,
							 to,
							 interval,
							 reverse = FALSE,
							 units = c("secs", "mins", "hours", "days", "weeks")) {
	UseMethod("filter_flow_time")
}

#' @describeIn filter_flow_time Filters on flow time for a \code{\link[bupaR]{log}}.
#' @export
filter_flow_time.log <- function(log,
								 from,
								 to,
								 interval,
								 reverse = FALSE,
								 units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = T) || all(is.na(interval)) )) {
		stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
	}

	if(is.null(interval))
		stop("Provide an interval.")
	else { #if(!is.null(interval))
		lower_threshold <- interval[1]
		upper_threshold <- interval[2]
		lower_threshold <- ifelse(is.na(lower_threshold), -Inf, lower_threshold)
		upper_threshold <- ifelse(is.na(upper_threshold), Inf, upper_threshold)

		create_precedence_df(log) %>%
			mutate(across(c("next_activity","AID"), as.character)) %>%
			filter(.data[["AID"]] == from & .data[["next_activity"]] == to) %>%
			mutate(idle_time = as.double(.data[["next_start_time"]] - .data[["end_time"]], units = units)) %>%
			# filter for idle time between activities in the interval
			filter(between(idle_time, lower_threshold, upper_threshold)) %>%
			pull(.data[["CID"]]) %>%
			unique() -> case_selection

		filter_case(log = log, cases = case_selection, reverse)

	}
}

#' @describeIn filter_throughput_time Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_flow_time.grouped_log <- function(log,
										 from,
										 to,
										 interval,
										 reverse = FALSE,
										 units = c("secs", "mins", "hours", "days", "weeks"))
{


	bupaR:::apply_grouped_fun(log, fun = filter_flow_time.log, interval, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}
