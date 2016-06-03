
resource_frequency_resource <- function(eventlog) {
	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)

	eventlog %>%
		group_by_(resource_classifier, activity_instance_classifier) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/sum(absolute)) %>%
		arrange(-absolute)-> r

	return(r)
}
