resource_frequency_activity <- function(eventlog) {


	resource_classifier <- resource_id(eventlog)
	event_classifier <- activity_id(eventlog)
	case_classifier <- case_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)


	eventlog %>%
		group_by_(event_classifier, resource_classifier, activity_instance_classifier) %>%
		summarize() %>%
		summarize(freq = n()) %>%
		summarize(nr_of_activites= n(),
				  min = min(freq),
				  q1 = quantile(freq, 0.25),
				  median = median(freq),
				  mean = mean(freq),
				  q3 = quantile(freq, 0.75),
				  max = max(freq)) %>%
		return()


}
