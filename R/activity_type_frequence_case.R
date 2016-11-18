activity_type_frequency_case <- function(eventlog) {

	stop_eventlog(eventlog)

	case_classifier <- case_id(eventlog)

	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"

	s <- eventlog %>%
		group_by_(case_classifier) %>%
		summarize(absolute = n_distinct(event_classifier),
				  relative = n_distinct(event_classifier)/n())

	return(s)


}
