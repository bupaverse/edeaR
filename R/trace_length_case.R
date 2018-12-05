
trace_length_case <- function(eventlog) {

	eventlog %>%
		group_by_case %>%
		summarize(absolute = n_distinct(!!activity_instance_id_(eventlog)))
}
