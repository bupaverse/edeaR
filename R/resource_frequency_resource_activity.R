
resource_frequency_resource_activity <- function(eventlog) {
	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)
	event_classifier <- activity_id(eventlog)

	eventlog %>%
		group_by_(resource_classifier, event_classifier, activity_instance_classifier) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		group_by_(resource_classifier) %>%
		mutate(relative_resource = absolute/sum(absolute)) %>%
		group_by_(event_classifier) %>%
		mutate(relative_activity = absolute/sum(absolute)) %>%
		arrange(-absolute)-> r

	return(r)
}
