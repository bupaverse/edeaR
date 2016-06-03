activity_specialization_activity <- function(eventlog) {

	event_classifier <- activity_id(eventlog)
	resource_classifier <- resource_id(eventlog)

	eventlog %>%
		group_by_(event_classifier, resource_classifier) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(eventlog)) %>%
		arrange(-absolute) %>%
		return()
}
