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
									  from = NULL,
									  to = NULL,
									  interval = NULL,
									  #reverse = FALSE,
									  units = c("secs", "mins", "hours", "days", "weeks")) {
	UseMethod("filter_flow_time")
}

#' @describeIn filter_flow_time Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_flow_time.log <- function(log,
										  from = NULL,
										  to = NULL,
										  interval = NULL,
										  #reverse = FALSE,
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

		filter_case(log = log, cases = case_selection) #, reverse)

		# log %>%
		# 	#filter cases on directly follows relationship
		# 	filter_precedence(antecedents = from, consequents = to) %>%
		# 	#calculate processing time between the activities
		# 	group_by_case() %>%
		# 	mutate(processing_time_between_all_activities = last(time) - first(time),
		# 		   processing_time_between_all_activities = as.numeric(processing_time_between_all_activities)) %>%
		# 	ungroup() %>%
		# 	filter(between(processing_time_between_all_activities, lower_threshold, upper_threshold)) %>%
		# 	re_map(mapping(patients)) %>%
		# 	pull(case_id(patients)) -> case_selection
		#
		# filter_case(log = log, cases = case_selection) #, reverse)
	}
}

	# # KLAD
	# processmapR:::create_base_precedence(patients,
	# 									 type_nodes = processmapR::performance(),
	# 									 type_edges = processmapR::performance()) -> base_precedence
	# base_precedence %>%
	# 	filter(ACTIVITY_CLASSIFIER_ == "Triage and Assessment" & next_act == "Blood test") %>%
	# 	mutate(idle_time = difftime(next_start_time, end_time, units = "hours"),
	# 		   idle_time = as.double(idle_time)) %>%
	# 	filter(between(idle_time, 10, 25)) %>%
	# 	pull(CASE_CLASSIFIER_) -> x
	#
	# filter_case(patients, x) %>% activities
	#
