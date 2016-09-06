
resource_frequency_log <- function(eventlog) {

	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)

	eventlog %>%
		group_by_(resource_classifier, activity_instance_classifier) %>%
		summarize() %>%
		summarize(freq = n()) -> r

	s <- summary_statistics(r$freq)

	return(s)
}
