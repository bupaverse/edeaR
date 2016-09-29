
trace_length_case <- function(eventlog) {

	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"

	eventlog %>%
		group_by_(case_id(eventlog)) %>%
		summarize(trace_length = n_distinct(activity_instance_classifier)) %>%
	return()
}
