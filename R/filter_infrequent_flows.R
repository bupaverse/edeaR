#' @title Filter Infrequent Flows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filter cases based on infrequent flows.
#'
#' @param min_n \code{numeric}: Cases containing a flow that occurs less than \code{min_n} times are discarded.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export
filter_infrequent_flows <- function(log, min_n, eventlog = deprecated()) {
	UseMethod("filter_infrequent_flows")
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_infrequent_flows.eventlog <- function(log, min_n, eventlog = deprecated()) {

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

	flow_info %>%
		filter(n < min_n) %>%
		unnest(data) %>%
		pull(!!case_id_(log)) %>%
		unique() -> remove

	log <- filter_case.log(log, cases = remove, reverse = TRUE)

	return(log)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_infrequent_flows.grouped_eventlog <- function(log, min_n, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, filter_infrequent_flows.eventlog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an \code{\link[bupaR]{activitylog}}.
#' @export
filter_infrequent_flows.activitylog <- function(log, min_n, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	filter_infrequent_flows.eventlog(bupaR::to_eventlog(log), min_n = min_n) %>%
		bupaR::to_activitylog()
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
filter_infrequent_flows.grouped_activitylog <- function(log, min_n, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, filter_infrequent_flows.activitylog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}