
end_activities_resource_activity <- function(eventlog) {

	resource_classifier <- resource_id(eventlog)
	case_classifier <- case_id(eventlog)
	event_classifier <- activity_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"

	eventlog %>%
		group_by_(case_classifier) %>%
		mutate(rank = row_number(timestamp_classifier)) %>%
		filter(rank == 1) %>%
		group_by_(resource_classifier, event_classifier) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) %>%
	return()

}
