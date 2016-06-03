resource_specialization_resource <- function(eventlog) {


	resource_classifier <- resource_id(eventlog)
	event_classifier <- activity_id(eventlog)


	eventlog %>%
		group_by_(resource_classifier, event_classifier) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_activities(eventlog)) %>%
		arrange(-absolute) %>%
		return()
	}
