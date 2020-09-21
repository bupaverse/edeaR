#' Filter cases based on infrequent flows.
#'
#' @param eventlog The event log to use
#' @param min_n Cases containing a flow that occurs less than min_n times are discarded.
#'
#' @return Filtered event log.
#' @export
#'

filter_infrequent_flows <- function(eventlog, min_n) {
	UseMethod("filter_infrequent_flows")
}

#' @export

filter_infrequent_flows.eventlog <- function(eventlog, min_n) {

	if(min_n <= 1) {
		stop("min_n should be at least 2")
	}

	eventlog %>%
		add_start_activity("START_ACT") %>%
		create_minimal_activity_log() %>%
		select(case_id(eventlog), activity_id(eventlog), timestamp(eventlog), .order) %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(!!timestamp_(eventlog)) %>%
		mutate(next_act = lead(!!activity_id_(eventlog), default = "END_ACT")) %>%
		select(!!case_id_(eventlog), !!activity_id_(eventlog), next_act) %>%
		nest(data = c(!!case_id_(eventlog))) %>%
		mutate(n = map_int(data, nrow)) -> flow_info


	flow_info %>%
		filter(n < min_n) %>%
		unnest(data) %>%
		pull(!!case_id_(eventlog)) %>%
		unique() -> remove

	log <- filter_case(eventlog, remove, reverse = T)

	return(log)
}
