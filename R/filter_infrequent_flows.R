#' @title Filter Infrequent Flows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filter cases based on infrequent flows.
#'
#' @param min_n [`numeric`] (default `2`): Cases containing a flow that occurs less than `min_n` times are discarded.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export
filter_infrequent_flows <- function(log, min_n = 2, eventlog = deprecated()) {
	UseMethod("filter_infrequent_flows")
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an [`eventlog`][`bupaR::eventlog`].
#' @export
filter_infrequent_flows.eventlog <- function(log, min_n = 2, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_lifecycle(eventlog)",
			with = "filter_lifecycle(log)")
		log <- eventlog
	}

	next_act <- NULL

	if(min_n <= 1) {
		stop("min_n should be at least 2")
	}

	log %>%
		add_start_activity("START_ACT") %>%
		create_minimal_activity_log() %>%
		select(case_id(log), activity_id(log), timestamp(log), .order) %>%
		group_by(!!case_id_(log)) %>%
		arrange(!!timestamp_(log)) %>%
		mutate(next_act = lead(!!activity_id_(log), default = "END_ACT")) %>%
		select(!!case_id_(log), !!activity_id_(log), next_act) %>%
		nest(data = c(!!case_id_(log))) %>%
		mutate(n = map_int(data, nrow)) -> flow_info

	# log %>%
	# 	add_start_activity("START_ACT") %>%
	# 	data.table() -> dt
	#
	# # For each case, keep only events with minimum timestamp and .order
	# cols <- c(case_id(log), activity_id(log), timestamp(log), ".order")
	# setorderv(dt, cols = c(case_id(log), timestamp(log), ".order"))
	# dt <- unique(dt, by = c(case_id(log), activity_instance_id(log), activity_id(log)))[,
	#              ..cols]
	#
	# # Order each case by timestamp
	# setorderv(dt, cols = c(case_id(log), timestamp(log)))


	flow_info %>%
		filter(n < min_n) %>%
		unnest(data) %>%
		pull(!!case_id_(log)) %>%
		unique() -> remove

	log <- filter_case.log(log, cases = remove, reverse = TRUE)

	return(log)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
filter_infrequent_flows.grouped_eventlog <- function(log, min_n = 2, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, filter_infrequent_flows.eventlog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an [`activitylog`][`bupaR::activitylog`].
#' @export
filter_infrequent_flows.activitylog <- function(log, min_n = 2, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	log %>%
		to_eventlog() %>%
		mutate(!!activity_instance_id(.) := as.character(.data[[activity_instance_id(.)]])) %>%
		filter_infrequent_flows.eventlog(min_n = min_n) %>%
		to_activitylog()
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
filter_infrequent_flows.grouped_activitylog <- function(log, min_n = 2, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, filter_infrequent_flows.activitylog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}