#' @title Filter directly follows with time interval
#'
#' @description Filter cases where the activity `from` is followed by activity `to` within a certain time `interval`.
#'
#' @param interval [`numeric`] vector of length 2: A duration interval. Half open interval can be created using [`NA`].
#' @param from,to [`character`] vector of length 1: The antecendent and consequent to filter on. Both are [`character`]
#' vectors containing exactly one activity identifier.
#' @param units [`character`] (default `"secs"`): The time unit in which the processing times should be reported. Should be one of the following values:
#' `"secs"` (default), `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units` argument of [`difftime()`].
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso [`processing_time()`],[`difftime()`]
#'
#' @family filters
#'
#' @concept filters_case
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

#' @describeIn filter_flow_time Filters on flow time for a [`bupaR::log`].
#' @export
filter_flow_time.log <- function(log,
								 from,
								 to,
								 interval,
								 reverse = FALSE,
								 units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = T) || all(is.na(interval)) )) {
		cli_abort(c("{.arg interval} should be a positive {.cls numeric} vector of length 2.",
		            "x" = "You supplied a {.cls {class(interval)}}: {.val {interval}}",
					"i" = "One of the elements can be {.code NA} to create open intervals."))
	}

	if(is.null(interval))
		cli_abort(c("Invalid {.arg interval}",
		            "x" = "{.arg interval} cannot be {.code NULL}"))
	else { #if(!is.null(interval))
		lower_threshold <- ifelse(is.na(interval[1]), -Inf, interval[1])
		upper_threshold <- ifelse(is.na(interval[2]), Inf, interval[2])

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

#' @describeIn filter_flow_time Filters on flow time for a [`bupaR::grouped_log`].
#' @export
filter_flow_time.grouped_log <- function(log,
										 from,
										 to,
										 interval,
										 reverse = FALSE,
										 units = c("secs", "mins", "hours", "days", "weeks")) {


	bupaR:::apply_grouped_fun(log, fun = filter_flow_time.log, interval, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}
