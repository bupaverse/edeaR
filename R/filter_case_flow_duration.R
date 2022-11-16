#' @title Filter Directly follows and Processing time
#'
#' @description Filters cases based on their \code{\link{processing_time}}.
#'
#' This filter can be used by using an \code{interval} or by using a \code{percentage}.
#' The percentage will always start with the shortest cases first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have a processing time in this interval. The time units
#' in which this interval is defined can be supplied with the \code{units} argument.
#'
#' @param interval,percentage Provide either \code{interval} or \code{percentage}.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): A duration interval. Half open interval can be created using \code{\link{NA}}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentage p to be used for relative filtering.
#'
#' @inherit filter_activity params references seealso return
#' @inherit processing_time params
#'
#' @seealso \code{\link{processing_time}}
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_case_flow_duration
filter_case_flow_duration <- function(log,
									  from = NULL,
									  to = NULL,
									  interval = NULL,
									  #reverse = FALSE,
									  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
									  eventlog = deprecated()) {
	UseMethod("filter_case_flow_duration")
}

#' @describeIn filter_processing_time Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_case_flow_duration.log <- function(log,
										  from = NULL,
										  to = NULL,
										  interval = NULL,
										  #reverse = FALSE,
										  units = c("auto", "secs", "mins", "hours", "days", "weeks")) { #eventlog = deprecated())

	# if(lifecycle::is_present(eventlog)) {
	#   lifecycle::deprecate_warn(
	#     when = "0.9.0",
	#     what = "filter_processing_time(eventlog)",
	#     with = "filter_processing_time(log)")
	#   log <- eventlog
	# }

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

		log %>%
			#filter cases on directly follows relationship
			filter_precedence(antecedents = from, consequents = to) %>%
			#calculate processing time between the activities
			group_by_case() %>%
			mutate(processing_time_between_all_activities = last(time) - first(time),
				   processing_time_between_all_activities = as.numeric(processing_time_between_all_activities)) %>%
			ungroup() %>%
			filter(between(processing_time_between_all_activities, lower_threshold, upper_threshold)) %>%
			re_map(mapping(patients)) %>%
			pull(case_id(patients)) -> case_selection

		filter_case(log = log, cases = case_selection) #, reverse)
	}
}
}
