
trace_length_case <- function(eventlog) {

	eventlog %>%
		group_by_case %>%
		summarize(trace_length = n_distinct(!!activity_instance_id_(eventlog)))
}
