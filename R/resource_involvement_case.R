resource_involvement_case <- function(eventlog) {

	case_classifier <- case_id(eventlog)
	resource_classifier <- resource_id(eventlog)

	eventlog %>%
		group_by_(case_classifier, resource_classifier) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(eventlog)) %>%
		arrange(-absolute) %>%
		return()
}
