resource_specialisation_case <- function(eventlog) {


	resource_classifier <- resource_id(eventlog)
	event_classifier <- activity_id(eventlog)
	case_classifier <- case_id(eventlog)

	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"

	eventlog %>%
		group_by_(case_classifier, resource_classifier, "event_classifier") %>%
		summarize() %>%
		group_by_(case_classifier) %>%
		mutate(nr_of_activity_types = n_distinct(event_classifier)) %>%
		group_by_(case_classifier, "nr_of_activity_types", resource_classifier) %>%
		summarize(freq = n()) %>%
		summarize( min = min(freq),
				  q1 = quantile(freq, 0.25),
				  median = median(freq),
				  mean = mean(freq),
				  q3 = quantile(freq, 0.75),
				  max = max(freq),
				  st_dev = sd(freq)) %>%
		mutate(iqr = q3 - q1) %>%
		return()
}
